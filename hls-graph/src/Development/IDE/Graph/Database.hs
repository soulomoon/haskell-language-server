module Development.IDE.Graph.Database(
    ShakeDatabase,
    ShakeValue,
    shakeNewDatabase,
    shakeRunDatabase,
    shakeRunDatabaseForKeys,
    shakeRunDatabaseForKeysSep,
    -- High-level helper: run with an action pump and enqueue upsweep for dirty keys
    shakeRunDatabaseForKeysSepWithPump,
    shakeProfileDatabase,
    shakeGetBuildStep,
    shakeGetDatabaseKeys,
    shakeGetDirtySet,
    shakeGetCleanKeys
    ,shakeGetBuildEdges,
    shakeShutDatabase,
    shakeGetActionQueueLength,
    shakeComputeToPreserve,
    -- shakedatabaseRuntimeDep,
    shakePeekAsyncsDelivers,
    upSweepAction,
    shakeGetTransitiveDirtyListBottomUp) where
import           Control.Concurrent.Async                (Async)
import           Control.Concurrent.Extra                (Barrier, newBarrier,
                                                          signalBarrier,
                                                          waitBarrierMaybe)
import           Control.Concurrent.STM.Stats            (atomically,
                                                          atomicallyNamed,
                                                          readTVarIO)
import           Control.Exception                       (SomeException, try)
import           Control.Monad                           (join, unless, void)
import           Control.Monad.IO.Class                  (liftIO)
import           Data.Dynamic
import           Data.Foldable                           (for_)
import           Data.Maybe
import           Data.Set                                (Set)
import           Data.Unique
import           Debug.Trace                             (traceEvent,
                                                          traceShowM)
import           Development.IDE.Graph.Classes           ()
import           Development.IDE.Graph.Internal.Action
import           Development.IDE.Graph.Internal.Database
import           Development.IDE.Graph.Internal.Key
import           Development.IDE.Graph.Internal.Options
import           Development.IDE.Graph.Internal.Profile  (writeProfile)
import           Development.IDE.Graph.Internal.Rules
import           Development.IDE.Graph.Internal.Types
import qualified Development.IDE.Graph.Internal.Types    as Logger
import           Development.IDE.WorkerThread            (DeliverStatus)
import           Extra                                   (offsetTime)


-- Placeholder to be the 'extra' if the user doesn't set it
data NonExportedType = NonExportedType

shakeShutDatabase :: Set (Async ()) -> ShakeDatabase -> IO ()
shakeShutDatabase preserve (ShakeDatabase _ _ db _) = shutDatabase preserve db

shakeNewDatabase :: (String -> IO ()) -> DBQue -> ActionQueue -> ShakeOptions -> Rules () -> IO ShakeDatabase
shakeNewDatabase l que aq opts rules = do
    let extra = fromMaybe (toDyn NonExportedType) $ shakeExtra opts
    (theRules, actions) <- runRules extra rules
    db <- newDatabase l que extra theRules
    pure $ ShakeDatabase (length actions) actions db aq

shakeRunDatabase :: ShakeDatabase -> [Action a] -> IO [Either SomeException a]
shakeRunDatabase s xs = shakeRunDatabaseForKeys Nothing s xs

-- | Returns the set of dirty keys annotated with their age (in # of builds)
shakeGetDirtySet :: ShakeDatabase -> IO [(Key, Int)]
shakeGetDirtySet (ShakeDatabase _ _ db _) =
    Development.IDE.Graph.Internal.Database.getDirtySet db

-- | Returns the build number
shakeGetBuildStep :: ShakeDatabase -> IO Int
shakeGetBuildStep (ShakeDatabase _ _ db _) = do
    Step s <- readTVarIO $ databaseStep db
    return s

-- Only valid if we never pull on the results, which we don't
unvoid :: Functor m => m () -> m a
unvoid = fmap undefined

-- | Assumes that the database is not running a build
-- The nested IO is to
-- seperate incrementing the step from running the build
shakeRunDatabaseForKeysSep
    :: Maybe (KeySet, KeySet) -- ^ Set of keys changed since last run. 'Nothing' means everything has changed
    -> ShakeDatabase
    -> [Action a]
    -> IO (IO [Either SomeException a])
shakeRunDatabaseForKeysSep keysChanged sdb as2 = shakeRunDatabaseForKeysSepWithPump keysChanged sdb as2

-- | Like 'shakeRunDatabaseForKeysSep', but also:
--   - runs an action pump that sequentially executes delayed actions from the given ActionQueue
--   - immediately enqueues upsweep actions for the newly dirty keys
-- This avoids duplicating this ceremony in callers that want to tightly couple the pump with the step increment.
shakeRunDatabaseForKeysSepWithPump
    :: Maybe (KeySet, KeySet)
    -> ShakeDatabase
    -> [Action a]
    -> IO (IO [Either SomeException a])
shakeRunDatabaseForKeysSepWithPump keysChanged (ShakeDatabase _ as1 db actionQueue) acts = do
    let runOne d = do
            getAction d
            liftIO $ atomically $ doneQueue d actionQueue

    let reenqUpsweep = case keysChanged of
            Nothing -> return ()
            Just (dirty, _) -> do
                for_ (toListKeySet dirty) $ \k -> do
                    (_, act) <- instantiateDelayedAction (mkDelayedAction ("upsweep" ++ show k) Debug $ upSweepAction k k)
                    atomically $ unGetQueue act actionQueue
                    -- insertRunnning
                -- return ()
                -- void $ atomically $ popAllQueue actionQueue
                -- todo why popAllQueue actionQueue won't work here?
    reenqueued <- atomicallyNamed "actionQueue - peek" $ peekInProgress actionQueue
    for_ reenqueued $ \d -> atomically $ unGetQueue d actionQueue
                -- return []
    traceEvent ("upsweep dirties " ++ show keysChanged) $ incDatabase db keysChanged
    reenqUpsweep
    -- let allActs = map (unvoid . runOne) reenqueued ++ acts
    return $ drop (length as1) <$> runActions (newKey "root") db (map unvoid as1 ++ acts)

instantiateDelayedAction
    :: DelayedAction a
    -> IO (Barrier (Either SomeException a), DelayedActionInternal)
instantiateDelayedAction (DelayedAction _ s p a) = do
  u <- newUnique
  b <- newBarrier
  let a' = do
        -- work gets reenqueued when the Shake session is restarted
        -- it can happen that a work item finished just as it was reenqueued
        -- in that case, skipping the work is fine
        alreadyDone <- liftIO $ isJust <$> waitBarrierMaybe b
        unless alreadyDone $ do
          x <- actionCatch @SomeException (Right <$> a) (pure . Left)
          -- ignore exceptions if the barrier has been filled concurrently
          liftIO $ void $ try @SomeException $ signalBarrier b x
      d' = DelayedAction (Just u) s p a'
  return (b, d')

mkDelayedAction :: String -> Logger.Priority -> Action a -> DelayedAction a
mkDelayedAction s p = DelayedAction Nothing s (toEnum (fromEnum p))



shakeComputeToPreserve :: ShakeDatabase -> KeySet -> IO ([(Key, Async ())], KeySet)
shakeComputeToPreserve (ShakeDatabase _ _ db _) ks = atomically (computeToPreserve db ks)

-- | Compute the transitive closure of the given keys over reverse dependencies
-- and return them in bottom-up order (children before parents).
shakeGetTransitiveDirtyListBottomUp :: ShakeDatabase -> [Key] -> IO [Key]
shakeGetTransitiveDirtyListBottomUp (ShakeDatabase _ _ db _) seeds =
    transitiveDirtyListBottomUp db seeds

-- fds make it possible to do al ot of jobs
shakeRunDatabaseForKeys
    :: Maybe [Key]
      -- ^ Set of keys changed since last run. 'Nothing' means everything has changed
    -> ShakeDatabase
    -> [Action a]
    -> IO [Either SomeException a]
shakeRunDatabaseForKeys Nothing sdb as2 = join $ shakeRunDatabaseForKeysSep Nothing sdb as2
shakeRunDatabaseForKeys (Just x) sdb as2 =
    let y = fromListKeySet x in join $ shakeRunDatabaseForKeysSep (Just (y, y)) sdb as2


shakePeekAsyncsDelivers :: ShakeDatabase -> IO [DeliverStatus]
shakePeekAsyncsDelivers (ShakeDatabase _ _ db _) = peekAsyncsDelivers db

-- | Given a 'ShakeDatabase', write an HTML profile to the given file about the latest run.
shakeProfileDatabase :: ShakeDatabase -> FilePath -> IO ()
shakeProfileDatabase (ShakeDatabase _ _ s _) file = writeProfile file s

-- | Returns the clean keys in the database
shakeGetCleanKeys :: ShakeDatabase -> IO [(Key, Result )]
shakeGetCleanKeys (ShakeDatabase _ _ db _) = do
    keys <- getDatabaseValues db
    return [ (k,res) | (k, Clean res) <- keys]

-- | Returns the total count of edges in the build graph
shakeGetBuildEdges :: ShakeDatabase -> IO Int
shakeGetBuildEdges (ShakeDatabase _ _ db _) = do
    keys <- getDatabaseValues db
    let ress = mapMaybe (getResult . snd) keys
    return $ sum $ map (lengthKeySet . getResultDepsDefault mempty . resultDeps) ress

-- | Returns an approximation of the database keys,
--   annotated with how long ago (in # builds) they were visited
shakeGetDatabaseKeys :: ShakeDatabase -> IO [(Key, Int)]
shakeGetDatabaseKeys (ShakeDatabase _ _ db _) = getKeysAndVisitAge db

shakeGetActionQueueLength :: ShakeDatabase -> IO Int
shakeGetActionQueueLength (ShakeDatabase _ _ _ aq) = do
    fromIntegral <$> atomically (countQueue aq)
