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
, Development.IDE.Graph.Internal.Action.getDirtySet
, getKeysAndVisitedAge
) where

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

-- parallel :: [Action a] -> Action [a]
-- parallel xs = parallel' xs

parallel :: String -> [Action a] -> Action [a]
parallel title [] = pure []
parallel title [x] = fmap (:[]) x
parallel title xs = do
    a <- Action ask
    deps <- liftIO $ readIORef $ actionDeps a
    case deps of
        UnknownDeps ->
            -- if we are already in the rerun mode, nothing we do is going to impact our state
            liftIO $ mapConcurrently (ignoreState title (length xs) a) (zip [0..] xs)
        deps -> do
            (newDeps, res) <- liftIO $ unzip <$> mapConcurrently (usingState a) xs
            liftIO $ writeIORef (actionDeps a) $ mconcat $ deps : newDeps
            pure res
    where
        usingState a x = do
            ref <- newIORef mempty
            res <- runReaderT (fromAction x) a{actionDeps=ref}
            deps <- readIORef ref
            pure (deps, res)

ignoreState :: String -> Int -> SAction -> (Int, Action b) -> IO b
ignoreState title n a (i, x) = do
    ref <- newIORef mempty
    runReaderT (fromAction x) a{actionDeps=ref} `catch` \(e :: SomeException) -> do
        liftIO $ traceEventIO $ "Build Action ignoreState exception " ++ title ++ ", " ++ "Num: " ++ show i ++"/" ++ show n ++" " ++ show e
        throw e

actionFork :: Action a -> (Async a -> Action b) -> Action b
actionFork act k = do
    a <- Action ask
    deps <- liftIO $ readIORef $ actionDeps a
    let db = actionDatabase a

    case deps of
        UnknownDeps -> do
            -- if we are already in the rerun mode, nothing we do is going to impact our state
            [res] <- liftIO $ withAsync (catchAndLogAndRethrow $ ignoreState "actionFork" (-1) a (-1, act)) $ \as -> runActions "actionFork" db [k as]
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

runActions :: String -> Database -> [Action a] -> IO [a]
runActions title db xs = do
    deps <- newIORef mempty
    step <- readTVarIO $ databaseStep db
    let tag i act =
            -- Log which action failed; rethrow unchanged so upstream semantics stay the same
            act `UE.catchAny` \(e :: SomeException) -> do
                liftIO $ traceEventIO ("Build runActions (" ++ title ++ ") action[" ++ show i ++ "] exception: " ++ show e)
                throw e
        tagged = zipWith tag ([0 ..] :: [Int]) xs
    runReaderT (fromAction $ parallel title tagged) (SAction db deps emptyStack) `catch` \(e :: SomeException) -> do
        traceEventIO $ "Build runActions " <> show step  <> ", " ++ title ++ ", Caught exception: " ++ show e
        throw e


-- | Returns the set of dirty keys annotated with their age (in # of builds)
getDirtySet  :: Action [(Key, Int)]
getDirtySet = do
    db <- getDatabase
    liftIO $ Development.IDE.Graph.Internal.Database.getDirtySet db

getKeysAndVisitedAge :: Action [(Key, Int)]
getKeysAndVisitedAge = do
    db <- getDatabase
    liftIO $ Development.IDE.Graph.Internal.Database.getKeysAndVisitAge db
