{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Development.IDE.Graph.Internal.Scheduler
  ( prepareToRunKey
  , prepareToRunKeys
  , decreasePendingCount
  , decreaseMyReverseDepsPendingCount
  , popOutDirtykeysDB
  , readReadyQueue
  , computeRunningNonBlocked
  , cleanHook
  , blockedOnThreadLimit
  , insertBlockedKey
  , prepareToRunKeysRealTime
  , writeUpsweepQueue
  , reportRemainDirties
  , reportTotalCount
  ) where

import           Control.Concurrent.STM               (STM, atomically, check,
                                                       flushTQueue, modifyTVar,
                                                       modifyTVar', readTQueue,
                                                       readTVar, writeTQueue,
                                                       writeTVar)
import           Control.Monad                        (forM, forM_, void, when)
import           Data.Maybe                           (fromMaybe)
import qualified StmContainers.Map                    as SMap

import           Debug.Trace                          (traceEvent)
import           Development.IDE.Graph.Internal.Key
import           Development.IDE.Graph.Internal.Types (Database (..),
                                                       KeyDetails (..),
                                                       Result (..), RunChanged,
                                                       SchedulerState (..),
                                                       Status (..), dbNotLocked,
                                                       getResult,
                                                       getResultDepsDefault)
import qualified StmContainers.Set                    as SSet


reportRemainDirties :: Database -> STM Int
reportRemainDirties (databaseScheduler -> SchedulerState{..}) =
    lengthKeySet <$> readTVar schedulerAllDirties

reportTotalCount :: Database -> STM Int
reportTotalCount (databaseScheduler -> SchedulerState{..}) =
    length <$> readTVar schedulerAllKeysInOrder

-- prepare to run a key in databaseDirtyTargets
-- we first peek if all the deps are clean
-- if so, we insert it into databaseRunningReady
-- otherwise, we insert it into databaseRunningPending with the pending count(the number of deps not clean)
-- so when a dep is cleaned, we can decrement the pending count, and when it reaches zero, we can move it to databaseRunningReady
prepareToRunKey :: Key -> Database -> STM ()
prepareToRunKey k Database {..} = do
  -- Determine the last known direct dependencies of k from its stored Result
  mKd <- SMap.lookup k databaseValues
  let deps = case mKd of
        Nothing -> mempty
        Just KeyDetails {keyStatus = st} ->
          let mRes = getResult st
           in maybe mempty (getResultDepsDefault mempty . resultDeps) mRes
      depList = filter (/= k) (toListKeySet deps)

  -- Peek dependency statuses to see how many are not yet clean
  depStatuses <- forM depList $ \d -> SMap.lookup d databaseValues
  let isCleanDep = \case
        Just KeyDetails {keyStatus = Clean _} -> True
        _ -> False
      pendingCount = length (filter (not . isCleanDep) depStatuses)

  let SchedulerState {..} = databaseScheduler
  if pendingCount == 0
    then do
      -- we need to know hat happens in the last time to determinie if something changed
      writeTQueue schedulerRunningReady k
      SMap.delete k schedulerRunningPending
    else do
      SMap.insert pendingCount k schedulerRunningPending


-- for key in the ready queue, if the parent key is running and the child key is not running,
-- it must be blocked on some new dependency
-- we insert the parent key into blocked set, and only clean it when its build succeedsb
insertBlockedKey :: String -> Key -> Key -> Database -> STM ()
insertBlockedKey reason pk k Database {..} = do
  let SchedulerState {..} = databaseScheduler
  return ()
--   isPkRunnings <- SSet.lookup pk schedulerRunningDirties
--   isKRunnings  <- SSet.lookup k schedulerRunningDirties
--   dirties <- readTVar schedulerAllDirties
  -- todo it might be blocked before we insert it into running
  -- and missing the insertion into blocked set when it actually runs
--   when (pk `memberKeySet` dirties && not isKRunnings) $ do
        -- SSet.delete pk schedulerRunningDirties
        -- SSet.insert pk schedulerRunningBlocked

-- take out all databaseDirtyTargets and prepare them to run
prepareToRunKeys :: Foldable t => Database -> t Key -> IO ()
prepareToRunKeys db dirtys = do
    forM_ dirtys $ \k -> atomically $ prepareToRunKey k db

prepareToRunKeysRealTime :: Database -> IO ()
prepareToRunKeysRealTime db@Database{..} = do
    -- pop one at a time to reduce fraction
    atomically $ do
        let SchedulerState{..} = databaseScheduler
        enque <- readTQueue schedulerUpsweepQueue
        prepareToRunKey enque db
    prepareToRunKeysRealTime db

-- decrease the pending count of a key in databaseRunningPending
-- if the pending count reaches zero, we move it to databaseRunningReady and remove it from databaseRunningPending
decreasePendingCount :: Key -> Database -> STM ()
decreasePendingCount k Database{..} = do
    let SchedulerState{..} = databaseScheduler
    mCount <- SMap.lookup k schedulerRunningPending
    case mCount of
        Nothing -> pure ()
        Just c
          | c <= 1 -> do
                -- Done waiting: move to ready and remove from pending
                SMap.delete k schedulerRunningPending
                writeTQueue schedulerRunningReady k
          | otherwise ->
                -- Decrement pending count
                SMap.insert (c - 1) k schedulerRunningPending

-- When a key becomes clean, decrement pending counters of its reverse dependents
-- gathered from both runtime and stored reverse maps
-- and remove it from runnning dirties and blocked sets
cleanHook :: Key -> Database -> STM ()
cleanHook k db = do
    -- remove itself from running dirties and blocked sets
    let SchedulerState{..} = databaseScheduler db
    -- SSet.delete k schedulerRunningDirties
    -- SSet.delete k schedulerRunningBlocked
    modifyTVar schedulerAllDirties $ deleteKeySet k

-- When a key becomes clean, decrement pending counters of its reverse dependents
-- gathered from both runtime and stored reverse maps.
decreaseMyReverseDepsPendingCount :: Key -> Database -> STM ()
decreaseMyReverseDepsPendingCount k db@Database{..} = do
    -- Gather reverse dependents from runtime map and stored reverse deps
    -- mStored  <- SMap.lookup k databaseValues
    mRuntime <- SMap.lookup k databaseRRuntimeDep
    let
        -- rdepsStored  = maybe mempty keyReverseDeps mStored
        rdepsRuntime = fromMaybe mempty mRuntime
        parents = deleteKeySet (newKey "root") rdepsRuntime
    -- For each parent, decrement its pending count; enqueue if it hits zero
    forM_ (toListKeySet parents) $ \p -> decreasePendingCount p db

writeUpsweepQueue :: [Key] -> Database -> STM ()
writeUpsweepQueue ks Database{..} = do
    let SchedulerState{..} = databaseScheduler
    forM_ ks $ \k -> writeTQueue schedulerUpsweepQueue k
    writeTVar schedulerAllKeysInOrder ks
    writeTVar schedulerAllDirties $ fromListKeySet ks

-- gather all dirty keys that is not finished, to reschedule after restart
-- includes keys in databaseDirtyTargets, databaseRunningReady, databaseRunningPending, databaseRunningDirties
-- and clears them from the database
popOutDirtykeysDB :: Database -> STM [Key]
popOutDirtykeysDB Database{..} = do
    let SchedulerState{..} = databaseScheduler
    -- 1. upsweep queue: drain all (atomic flush)
    void $ flushTQueue schedulerUpsweepQueue

    -- 2. Ready queue: drain all (atomic flush)
    void $ flushTQueue schedulerRunningReady

    -- 3. Pending map: collect keys and clear
    SMap.reset schedulerRunningPending

    -- 4. Running dirties set: read and clear
    -- runningDirties <- readTVar schedulerRunningDirties
    -- SSet.reset schedulerRunningDirties

    -- 5. Also clear blocked subset for consistency
    -- SSet.reset schedulerRunningBlocked

    -- 6. All dirties set: read and clear
    reenqueue <- readTVar schedulerAllDirties
    _ <- writeTVar schedulerAllDirties mempty
    allKeys <- readTVar schedulerAllKeysInOrder
    _ <- writeTVar schedulerAllKeysInOrder mempty
    pure $ filter (`memberKeySet` reenqueue) allKeys

-- read one key from ready queue, and insert it into running dirties
-- this function will block if there is no key in ready queue
-- and also block if the number of running non-blocked keys exceeds maxThreads
readReadyQueue :: Database -> STM Key
readReadyQueue db@Database{..} = do
    dbNotLocked db
    -- blockedOnThreadLimit db 32
    let SchedulerState{..} = databaseScheduler
    r <- readTQueue schedulerRunningReady
    -- is might blocked because it is already running by downsweep.
    -- isBlocked <- SSet.lookup r schedulerRunningBlocked
    -- if isBlocked
    --   then pure ()
    --   else SSet.insert r schedulerRunningDirties
    -- SSet.insert r schedulerRunningDirties
    return r


computeRunningNonBlocked :: Database -> STM Int
computeRunningNonBlocked Database{..} = do
    return 0
    -- let SchedulerState{..} = databaseScheduler
    -- runningSetSize <- SSet.size schedulerRunningDirties
    -- return runningSetSize

blockedOnThreadLimit :: Database -> Int -> STM ()
blockedOnThreadLimit db maxThreads = do
    runningNonBlocked <- computeRunningNonBlocked db
    check $ runningNonBlocked < maxThreads

