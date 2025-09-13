{-# LANGUAGE TypeFamilies #-}

module Development.IDE.Graph.Internal.Action
( ShakeValue
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
, runActionInDbCb
) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM.Stats            (atomicallyNamed)
import           Control.DeepSeq                         (force)
import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Foldable                           (toList)
import           Data.Functor.Identity
import           Data.IORef
import           Development.IDE.Graph.Classes
import           Development.IDE.Graph.Internal.Database
import           Development.IDE.Graph.Internal.Key
import           Development.IDE.Graph.Internal.Rules    (RuleResult)
import           Development.IDE.Graph.Internal.Types
import           Development.IDE.WorkerThread
import           System.Exit
import           UnliftIO                                (STM, atomically,
                                                          newEmptyTMVarIO,
                                                          putTMVar, readTMVar)

type ShakeValue a = (Show a, Typeable a, Eq a, Hashable a, NFData a)

-- | Always rerun this rule when dirty, regardless of the dependencies.
alwaysRerun :: Action ()
alwaysRerun = do
    ref <- Action $ asks actionDeps
    liftIO $ modifyIORef' ref (AlwaysRerunDeps mempty <>)

parallel :: [Action a] -> Action [Either SomeException a]
parallel [] = return []
parallel xs = do
    a <- Action ask
    deps <- liftIO $ readIORef $ actionDeps a
    case deps of
        UnknownDeps ->
            -- if we are already in the rerun mode, nothing we do is going to impact our state
            runActionInDb xs
        deps -> error $ "parallel not supported when we have precise dependencies: " ++ show deps
            -- (newDeps, res) <- liftIO $ unzip <$> runActionInDb usingState xs
            -- liftIO $ writeIORef (actionDeps a) $ mconcat $ deps : newDeps
            -- return ()

-- non-blocking version of runActionInDb
runActionInDbCb :: (a -> Key) -> (a -> Action result) -> STM a -> (Either SomeException result -> IO ()) -> Action a
runActionInDbCb getTitle run getAct handler = do
    a <- Action ask
    liftIO $ atomicallyNamed "action queue - pop" $ do
        act <- getAct
        let key = getTitle act
        -- might be not keep across session
        runInDataBase (do
            s <- atomically $ getDataBaseStepInt (actionDatabase a)
            return $ DeliverStatus s key
            ) (actionDatabase a) [(ignoreState a $ setActionKey key $ run act, handler)]
        return act

runActionInDb :: [Action a] -> Action [Either SomeException a]
runActionInDb acts = do
    a <- Action ask
    xs <- mapM (\x -> do
        barrier <- newEmptyTMVarIO
        return (x, barrier)) acts
    -- always rerun !
    s <- atomically $ getDataBaseStepInt (actionDatabase a)
    liftIO $ atomically $ runInDataBase (return $ DeliverStatus s (actionKey a)) (actionDatabase a)
        (map (\(x, b) -> (ignoreState a x, atomically . putTMVar b)) xs)
    results <- liftIO $ mapM (atomically . readTMVar) $ fmap snd xs
    return results

ignoreState :: SAction -> Action b -> IO b
ignoreState a x = do
    ref <- newIORef mempty
    runReaderT (fromAction x) a{actionDeps=ref}

isAsyncException :: SomeException -> Bool
isAsyncException e
    | Just (_ :: SomeAsyncException) <- fromException e = True
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
    pk <- getActionKey
    (is, vs) <- liftIO $ build pk db stack ks
    ref <- Action $ asks actionDeps
    let !ks = force $ fromListKeySet $ toList is
    liftIO $ modifyIORef' ref (ResultDeps [ks] <>)
    pure vs

-- | Evaluate a list of keys without recording any dependencies.
applyWithoutDependency :: (Traversable f, RuleResult key ~ value, ShakeValue key, Typeable value) => f key -> Action (f value)
applyWithoutDependency ks = do
    db <- Action $ asks actionDatabase
    stack <- Action $ asks actionStack
    pk <- getActionKey
    (_, vs) <- liftIO $ build pk db stack ks
    pure vs

runActions :: Key -> Database -> [Action a] -> IO [Either SomeException a]
runActions pk db xs = do
    deps <- newIORef mempty
    runReaderT (fromAction $ parallel xs) $ SAction pk db deps emptyStack

-- | Returns the set of dirty keys annotated with their age (in # of builds)
getDirtySet  :: Action [(Key, Int)]
getDirtySet = do
    db <- getDatabase
    liftIO $ Development.IDE.Graph.Internal.Database.getDirtySet db

getKeysAndVisitedAge :: Action [(Key, Int)]
getKeysAndVisitedAge = do
    db <- getDatabase
    liftIO $ Development.IDE.Graph.Internal.Database.getKeysAndVisitAge db
