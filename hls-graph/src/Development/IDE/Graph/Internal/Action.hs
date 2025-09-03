{-# LANGUAGE TypeFamilies #-}

module Development.IDE.Graph.Internal.Action
( ShakeValue
, actionFork
, actionBracket
, actionCatch
, actionFinally
, alwaysRerun
, apply1
, apply
, applyWithoutDependency
, parallel
, runActions
, mapConcurrentlyAPKill
, Development.IDE.Graph.Internal.Action.getDirtySet
, getKeysAndVisitedAge
) where

import           Control.Concurrent                      (myThreadId)
import           Control.Concurrent.Async
import           Control.Concurrent.STM                  (readTVarIO)
import           Control.DeepSeq                         (force)
import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Foldable                           (toList)
import           Data.Functor.Identity
import           Data.IORef
import           Debug.Trace                             (traceEventIO)
import           Development.IDE.Graph.Classes
import           Development.IDE.Graph.Internal.Database
import           Development.IDE.Graph.Internal.Key
import           Development.IDE.Graph.Internal.Rules    (RuleResult)
import           Development.IDE.Graph.Internal.Types
import           System.Exit
import qualified UnliftIO.Exception                      as UE
-- (no STM imports needed here)

type ShakeValue a = (Show a, Typeable a, Eq a, Hashable a, NFData a)

-- | Always rerun this rule when dirty, regardless of the dependencies.
alwaysRerun :: Action ()
alwaysRerun = do
    ref <- Action $ asks actionDeps
    liftIO $ modifyIORef' ref (AlwaysRerunDeps mempty <>)


-- parallel :: String -> [(String, Action a)] -> Action [a]
parallel _ title [] = pure []
parallel _ title [x] = fmap (:[]) (snd x)
parallel step title xs = do
    a <- Action ask
    deps <- liftIO $ readIORef $ actionDeps a
    case deps of
        UnknownDeps ->
            -- if we are already in the rerun mode, nothing we do is going to impact our state
            liftIO $ mapConcurrentlyAPKill step title (ignoreState title (length xs) a) (zip [0..] xs)
        deps -> do
            error "please help me"
            -- (newDeps, res) <- liftIO $ unzip <$> mapConcurrently (usingState a) xs
            -- liftIO $ writeIORef (actionDeps a) $ mconcat $ deps : newDeps
            -- pure res
    where
        usingState a (name, x) = do
            ref <- newIORef mempty
            res <- runReaderT (fromAction x) a{actionDeps=ref}
            deps <- readIORef ref
            pure (deps, res)

ignoreState :: String -> Int -> SAction -> (Int, (String, Action b)) -> IO b
ignoreState title n a (i, (name, x)) = do
    ref <- newIORef mempty
    step <- readTVarIO $ databaseStep $ actionDatabase a
    ttid <- myThreadId
    runReaderT (fromAction x) a{actionDeps=ref} `catch` \(e :: SomeException) -> do
        let newException = case fromException e of
                Just (AsyncParentKill tid s t) -> AsyncParentKill tid s (t <> formatName step ttid "ignoreState" <> show title ++ "[" <> name <> "]" )
                _ -> AsyncParentKill ttid step (show e <> formatName step ttid "ignoreState" <> show title ++ "[" <> name <> "]" )
        liftIO $ traceEventIO $ "Build ignoreState " ++ title ++ "[" <> name <> "], " ++ "Num: " ++ show i ++"/" ++ show n ++" " ++ show newException
        throw newException

actionFork :: (String, Action a) -> (Async a -> Action b) -> Action b
actionFork (name, act) k = do
    a <- Action ask
    deps <- liftIO $ readIORef $ actionDeps a
    let db = actionDatabase a
    step <- liftIO $ readTVarIO $ databaseStep db
    tid <- liftIO myThreadId

    case deps of
        UnknownDeps -> do
            -- if we are already in the rerun mode, nothing we do is going to impact our state
            [res] <- liftIO $ do
                as <- async $ catchAndLogAndRethrow $ ignoreState "actionFork" (-1) a (-1, (name, act))
                runActions "actionFork" db [(name, k as)] `catch` \(e :: SomeException) -> do
                    traceEventIO $ "Build Action actionFork Caught exception: " ++ show e
                    let newError = AsyncParentKill tid step (show e <> " " <> "actionFork" ++ "[" <> name <> "]")
                    cancelWith as newError
                    throw newError

            return res
        _ ->
            error "please help me"

-- Ensure we keep a signature to satisfy -Werror=missing-signatures
catchAndLogAndRethrow :: IO b -> IO b
catchAndLogAndRethrow a = a `UE.catch` \e -> do
    liftIO $ traceEventIO $ "Build Action catchAndLogAndRethrow: " ++ show (e :: SomeException)
    throw e

isAsyncException :: SomeException -> Bool
isAsyncException e
    | Just (_ :: AsyncCancelled) <- fromException e = True
    | Just (_ :: AsyncException) <- fromException e = True
    | Just (_ :: AsyncParentKill) <- fromException e = True
    | Just (_ :: ExitCode) <- fromException e = True
    | otherwise = False


actionCatch :: Exception e => Action a -> (e -> Action a) -> Action a
actionCatch a b = do
    v <- Action ask
    Action $ lift $ catchJust f (runReaderT (fromAction a) v) (\x -> runReaderT (fromAction (b x)) v)
    where
        -- Catch only catches exceptions that were caused by this code, not those that
        -- are a result of program termination
        f e | isAsyncException e = Nothing
            | otherwise = fromException e

actionBracket :: IO a -> (a -> IO b) -> (a -> Action c) -> Action c
actionBracket a b c = do
    v <- Action ask
    Action $ lift $ bracket a b (\x -> runReaderT (fromAction (c x)) v)

actionFinally :: Action a -> IO b -> Action a
actionFinally a b = do
    v <- Action ask
    Action $ lift $ finally (runReaderT (fromAction a) v) b

apply1 :: (RuleResult key ~ value, ShakeValue key, Typeable value) => key -> Action value
apply1 k = runIdentity <$> apply (Identity k)

apply :: (Traversable f, RuleResult key ~ value, ShakeValue key, Typeable value) => f key -> Action (f value)
apply ks = do
    db <- Action $ asks actionDatabase
    stack <- Action $ asks actionStack
    (is, vs) <- liftIO $ build db stack ks
    ref <- Action $ asks actionDeps
    let !ks = force $ fromListKeySet $ toList is
    liftIO $ modifyIORef' ref (ResultDeps [ks] <>)
    pure vs

-- | Evaluate a list of keys without recording any dependencies.
applyWithoutDependency :: (Traversable f, RuleResult key ~ value, ShakeValue key, Typeable value) => f key -> Action (f value)
applyWithoutDependency ks = do
    db <- Action $ asks actionDatabase
    stack <- Action $ asks actionStack
    (_, vs) <- liftIO $ build db stack ks
    pure vs

runActions :: String -> Database -> [(String, Action a)] -> IO [a]
runActions title db xs = do
    deps <- newIORef mempty
    step <- readTVarIO $ databaseStep db
    let tag i (n, act) =
            -- Log which action failed; rethrow unchanged so upstream semantics stay the same
            (n, act `UE.catchAny` \(e :: SomeException) -> do
                    liftIO $ traceEventIO ("Build runActions (" ++ title ++ ") action[" ++ show i ++ "] exception: " ++ show e)
                    throw e)
        tagged = zipWith tag ([0 ..] :: [Int]) xs
    runReaderT (fromAction $ parallel step title tagged) (SAction db deps emptyStack) `catch` \(e :: SomeException) -> do
        traceEventIO $ "Build runActions " <> show step  <> ", " ++ title ++ ", Caught exception: " ++ show e
        throw e


-- | Like 'mapConcurrently', but if any worker fails, cancels the rest by
--   throwing 'AsyncParentKill' to them. Returns results in the original order
--   if all workers succeed. The label is included in the kill exception.
mapConcurrentlyAPKill :: (Traversable t) => Step -> String -> (a -> IO b) -> t a -> IO (t b)
mapConcurrentlyAPKill step label f xs = uninterruptibleMask $ \restore -> do
  asStruct <- traverse (\x -> async $ restore (f x)) xs
  let asList = toList asStruct
  let loop :: [Async b] -> IO ()
      loop [] = pure ()
      loop pending = do
        ans <- (Right <$> restore (waitAnyCatch pending)) `catch` \(e :: SomeAsyncException) -> return $ Left e
        case ans of
          (Right (a, Right _)) -> loop (filter (/= a) pending)
          (Right (a, Left e)) -> do
            tid <- myThreadId
            let killEx = case fromException e of
                  Just (AsyncParentKill _ _ t) -> AsyncParentKill tid step (t <> formatName step tid "mapConcurrentlyAPKill" <> label)
                  _ -> AsyncParentKill tid step (show e <> formatName step tid "mapConcurrentlyAPKill" <> label)
                others = filter (/= a) pending
            mapM_ (\b -> throwTo (asyncThreadId b) killEx) others
            mapM_ waitCatch pending
            throwIO e
          Left e -> do
            tid <- myThreadId
            let killEx = case fromException $ asyncExceptionToException e of
                  Just (AsyncParentKill _ _ t) -> AsyncParentKill tid step (t <> formatName step tid "mapConcurrentlyAPKill" <> label)
                  _ -> AsyncParentKill tid step (show e <> formatName step tid "mapConcurrentlyAPKill" <> label)
            mapM_ (\b -> throwTo (asyncThreadId b) killEx) pending
            mapM_ waitCatch pending
            throwIO e
  loop asList
  traverse wait asStruct


-- | Returns the set of dirty keys annotated with their age (in # of builds)
getDirtySet  :: Action [(Key, Int)]
getDirtySet = do
    db <- getDatabase
    liftIO $ Development.IDE.Graph.Internal.Database.getDirtySet db

getKeysAndVisitedAge :: Action [(Key, Int)]
getKeysAndVisitedAge = do
    db <- getDatabase
    liftIO $ Development.IDE.Graph.Internal.Database.getKeysAndVisitAge db
