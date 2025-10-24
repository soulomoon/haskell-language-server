module Development.IDE.Graph.Database(
    ShakeDatabase,
    ShakeValue,
    shakeNewDatabase,
    shakeRunDatabase,
    shakeRunDatabaseForKeys,
    shakeRunDatabaseForKeysSep,
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
    instantiateDelayedAction,
    mkDelayedAction,
    upsweepAction,
    shakeDatabaseSize) where
import           Control.Concurrent.Extra                 (Barrier, newBarrier,
                                                           signalBarrier,
                                                           waitBarrierMaybe)
import           Control.Concurrent.STM.Stats             (atomically,
                                                           atomicallyNamed,
                                                           readTVarIO)
import           Control.Exception                        (SomeException, try)
import           Control.Monad                            (join, unless, void)
import           Control.Monad.IO.Class                   (liftIO)
import           Data.Dynamic
import           Data.Maybe
import           Data.Unique
import           Debug.Trace                              (traceEvent)
import           Development.IDE.Graph.Classes            ()
import           Development.IDE.Graph.Internal.Action
import           Development.IDE.Graph.Internal.Database
import           Development.IDE.Graph.Internal.Key
import           Development.IDE.Graph.Internal.Options
import           Development.IDE.Graph.Internal.Profile   (writeProfile)
import           Development.IDE.Graph.Internal.Rules
import           Development.IDE.Graph.Internal.Scheduler
import           Development.IDE.Graph.Internal.Types
import qualified Development.IDE.Graph.Internal.Types     as Logger
import           Development.IDE.WorkerThread             (DeliverStatus)
import qualified StmContainers.Map                        as SMap
import           System.Time.Extra                        (duration,
                                                           showDuration)


-- Placeholder to be the 'extra' if the user doesn't set it
data NonExportedType = NonExportedType

shakeShutDatabase :: KeySet -> ShakeDatabase -> IO ()
shakeShutDatabase dirties (ShakeDatabase _ _ db) = shutDatabase dirties db

shakeNewDatabase :: (String -> IO ()) -> DBQue -> ActionQueue -> ShakeOptions -> Rules () -> IO ShakeDatabase
shakeNewDatabase l que aq opts rules = do
    let extra = fromMaybe (toDyn NonExportedType) $ shakeExtra opts
    (theRules, actions) <- runRules extra rules
    db <- newDatabase l que aq extra theRules
    pure $ ShakeDatabase (length actions) actions db

shakeRunDatabase :: ShakeDatabase -> [Action a] -> IO [Either SomeException a]
shakeRunDatabase s xs = shakeRunDatabaseForKeys Nothing s xs

-- | Returns the set of dirty keys annotated with their age (in # of builds)
shakeGetDirtySet :: ShakeDatabase -> IO [(Key, Int)]
shakeGetDirtySet (ShakeDatabase _ _ db) =
    Development.IDE.Graph.Internal.Database.getDirtySet db

-- | Returns the build number
shakeGetBuildStep :: ShakeDatabase -> IO Int
shakeGetBuildStep (ShakeDatabase _ _ db) = do
    Step s <- readTVarIO $ databaseStep db
    return s

-- Only valid if we never pull on the results, which we don't
unvoid :: Functor m => m () -> m a
unvoid = fmap undefined

-- | Assumes that the database is not running a build
-- The nested IO is to
-- seperate incrementing the step from running the build.
-- Also immediately enqueues upsweep actions for the newly dirty keys.
shakeRunDatabaseForKeysSep
    :: Maybe (([Key],[Key]),KeySet) -- ^ Set of keys changed since last run. 'Nothing' means everything has changed
    -> ShakeDatabase
    -> [Action a]
    -> IO (IO [Either SomeException a])
shakeRunDatabaseForKeysSep keysChanged sdb@(ShakeDatabase _ as1 db) acts = do
    -- we can to upsweep these keys in order one by one,
    preserves <- traceEvent ("upsweep dirties " ++ show keysChanged) $ incDatabase db keysChanged
    -- (_, act) <- instantiateDelayedAction =<< (mkDelayedAction "upsweep" Debug $ upsweepAction)
    reenqueued <- atomicallyNamed "actionQueue - peek" $ peekInProgress (databaseActionQueue db)
    let reenqueuedExceptPreserves = filter (\d -> uniqueID d `notMemberKeySet` preserves) reenqueued
    -- let ignoreResultActs = (getAction act) : (liftIO $ prepareToRunKeysRealTime db) : as1
    -- let ignoreResultActs = (getAction act) : as1
    let ignoreResultActs = as1
    return $ do
        -- (tm, keys) <- duration $ prepareToRunKeys db
        -- dataBaseLogger db $ "prepareToRunKeys took " ++ showDuration tm ++ " for " ++ show (length keys) ++ " keys"
        seqRunActions (newKey "root") db $ map (pumpActionThreadReRun sdb) reenqueuedExceptPreserves
        drop (length ignoreResultActs) <$> runActions (newKey "root") db (map unvoid ignoreResultActs ++ acts)

instantiateDelayedAction
    :: DelayedAction a
    -> IO (Barrier (Either SomeException a), DelayedActionInternal)
instantiateDelayedAction (DelayedAction u s p a) = do
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
      d' = DelayedAction u s p a'
  return (b, d')

mkDelayedAction :: String -> Logger.Priority -> Action a -> IO (DelayedAction a)
mkDelayedAction s p a = do
    u <- newUnique
    return $ DelayedAction (newDirectKey $ hashUnique u) s (toEnum (fromEnum p)) a

-- shakeComputeToPreserve :: ShakeDatabase -> KeySet -> IO (KeySet, ([Key], [Key]), Int)
shakeComputeToPreserve :: ShakeDatabase -> KeySet -> IO (KeySet, ([Key], [Key]), Int, [Key])
shakeComputeToPreserve (ShakeDatabase _ _ db) ks = atomically (computeToPreserve db ks)

-- fds make it possible to do al ot of jobs
shakeRunDatabaseForKeys
    :: Maybe [Key]
      -- ^ Set of keys changed since last run. 'Nothing' means everything has changed
    -> ShakeDatabase
    -> [Action a]
    -> IO [Either SomeException a]
shakeRunDatabaseForKeys Nothing sdb as2 = join $ shakeRunDatabaseForKeysSep Nothing sdb as2
shakeRunDatabaseForKeys (Just x) sdb as2 =
    let y = fromListKeySet x in join $ shakeRunDatabaseForKeysSep (Just (([], toListKeySet y), y)) sdb as2


shakePeekAsyncsDelivers :: ShakeDatabase -> IO [DeliverStatus]
shakePeekAsyncsDelivers (ShakeDatabase _ _ db) = peekAsyncsDelivers db

shakeDatabaseSize :: ShakeDatabase -> IO Int
shakeDatabaseSize (ShakeDatabase _ _ db) = databaseSize db

databaseSize :: Database -> IO Int
databaseSize db = atomically $ SMap.size $ databaseValues db

-- | Given a 'ShakeDatabase', write an HTML profile to the given file about the latest run.
shakeProfileDatabase :: ShakeDatabase -> FilePath -> IO ()
shakeProfileDatabase (ShakeDatabase _ _ db) file = writeProfile file db

-- | Returns the clean keys in the database
shakeGetCleanKeys :: ShakeDatabase -> IO [(Key, Result )]
shakeGetCleanKeys (ShakeDatabase _ _ db) = do
    keys <- getDatabaseValues db
    return [ (k,res) | (k, Clean res) <- keys]

-- | Returns the total count of edges in the build graph
shakeGetBuildEdges :: ShakeDatabase -> IO Int
shakeGetBuildEdges (ShakeDatabase _ _ db) = do
    keys <- getDatabaseValues db
    let ress = mapMaybe (getResult . snd) keys
    return $ sum $ map (lengthKeySet . getResultDepsDefault mempty . resultDeps) ress

-- | Returns an approximation of the database keys,
--   annotated with how long ago (in # builds) they were visited
shakeGetDatabaseKeys :: ShakeDatabase -> IO [(Key, Int)]
shakeGetDatabaseKeys (ShakeDatabase _ _ db) = getKeysAndVisitAge db

shakeGetActionQueueLength :: ShakeDatabase -> IO Int
shakeGetActionQueueLength (ShakeDatabase _ _ db) = do
    fromIntegral <$> atomically (countQueue (databaseActionQueue db))
