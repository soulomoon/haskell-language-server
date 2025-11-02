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
  , cleanHook
  , prepareToRunKeysRealTime
  , writeUpsweepQueue
  , reportRemainDirties
  , reportTotalCount
  , isDirty
  , isRunDepChangedOne
  ) where

import           Control.Concurrent.STM               (STM, atomically,
                                                       flushTQueue, readTQueue,
                                                       readTVar, writeTQueue,
                                                       writeTVar)
import           Control.Monad                        (filterM, foldM, forM_,
                                                       void)
import           Data.Maybe                           (fromMaybe)
import qualified StmContainers.Map                    as SMap

import qualified Control.Concurrent.STM.TPQueue       as TPQ
import           Data.Foldable                        (Foldable (..))
import           Development.IDE.Graph.Internal.Key
import           Development.IDE.Graph.Internal.Types (Database (..),
                                                       KeyDetails (..),
                                                       Result (..),
                                                       ResultDeps (..),
                                                       RunMode (..),
                                                       SchedulerState (..),
                                                       Status (..), dbNotLocked,
                                                       lookupDatabaseRuntimeDepRootCounter)
import qualified StmContainers.Set                    as SSet

type StatusCache = KeyMap (Maybe KeyDetails)

-- | Cache lookups into 'databaseValues' during a batch to avoid repeated STM work.
lookupStatusCache :: Database -> Key -> StatusCache -> STM (Maybe KeyDetails, StatusCache)
lookupStatusCache Database{..} k cache =
  case lookupKeyMap k cache of
    Just v  -> pure (v, cache)
    Nothing -> do
      v <- SMap.lookup k databaseValues
      pure (v, insertKeyMap k v cache)

reportRemainDirties :: Database -> STM Int
reportRemainDirties (databaseScheduler -> SchedulerState{..}) =
    SSet.size schedulerAllDirties

reportTotalCount :: Database -> STM Int
reportTotalCount (databaseScheduler -> SchedulerState{..}) =
    length <$> readTVar schedulerAllKeysInOrder

-- | isDirty
-- only dirty when it's build time is older than the changed time of one of its dependencies
isDirty :: Foldable t => Result -> t (a, Result) -> Bool
isDirty me = any (\(_,dep) -> resultBuilt me < resultChanged dep)

isRunDepChangedOne :: Result -> Result -> RunMode
isRunDepChangedOne me dep =
    if resultBuilt me < resultChanged dep then RunDependenciesChanged else RunDependenciesSame

-- prepare to run a key in databaseDirtyTargets
-- we first peek if all the deps are clean
-- if so, we insert it into databaseRunningReady
-- otherwise, we insert it into databaseRunningPending with the pending count(the number of deps not clean)
-- so when a dep is cleaned, we can decrement the pending count, and when it reaches zero, we can move it to databaseRunningReady
prepareToRunKey :: Key -> Database -> STM ()
prepareToRunKey key db =
    void $ prepareToRunKeyCached db mempty key

prepareToRunKeyCached :: Database -> StatusCache -> Key -> STM StatusCache
prepareToRunKeyCached db@Database {..} cache0 key = do
    let SchedulerState {..} = databaseScheduler
    (status, cache1) <- lookupStatusCache db key cache0
    (cache2, res) <- case keyStatus <$> status of
        Just (Dirty Nothing) -> pure (cache1, Just (0, RunDependenciesChanged, Nothing))
        Just (Dirty (Just r)) -> do
            -- todo we use final deps instead of runtime deps here
            -- does it cause in compatiable issues?
            -- we did not take care of always rerun here
            let depsSet =
                    case resultDeps r of
                        ResultDeps deps -> fold deps
                        _               -> mempty
            if nullKeySet depsSet
                then pure (cache1, Just (0, RunDependenciesChanged, Just r))
                else do
                    let totalDeps = lengthKeySet depsSet
                    let depsList = toListKeySet depsSet
                    (cacheFinal, cleanCount, runMode) <- foldM (collectDep r) (cache1, 0, RunDependenciesSame) depsList
                    let pendingCount = totalDeps - cleanCount
                    pure (cacheFinal, Just (pendingCount, runMode, Just r))
        -- s -> trace ("prepareToRunKey: key " ++ show key ++ " is not dirty but in dirty targets, status: " ++ show s) $ cleanHook key db >> return Nothing
        --  todo find out how to avoid this
        -- this is possible when a key still downsweeping
        -- we leave it for the downsweep to handle
        -- since it is not upsweep responsibility
        _ -> cleanHook key db >> pure (cache1, Nothing)
    case res of
        Nothing -> pure cache2
        Just (pendingCount, runMode, mRes) ->
            if pendingCount == 0
                then do
                    prio <- lookupDatabaseRuntimeDepRootCounter key db
                    TPQ.writeTPQueue schedulerRunningReady prio (key, runMode, mRes)
                    SMap.delete key schedulerRunningPending
                    pure cache2
                else do
                    SMap.insert (pendingCount, runMode, mRes) key schedulerRunningPending
                    pure cache2
  where
    collectDep r (cacheAcc, cleanAcc, modeAcc) dep = do
        (depStatus, cacheNext) <- lookupStatusCache db dep cacheAcc
        case depStatus of
            Just KeyDetails {keyStatus = Clean res} ->
                let cleanAcc' = cleanAcc + 1
                    modeAcc'  = modeAcc <> isRunDepChangedOne r res
                in pure (cacheNext, cleanAcc', modeAcc')
            _ -> pure (cacheNext, cleanAcc, modeAcc)


-- take out all databaseDirtyTargets and prepare them to run
prepareToRunKeys :: Database -> IO [Key]
prepareToRunKeys db =
    atomically $ do
        dbNotLocked db
        let SchedulerState{..} = databaseScheduler db
        dirtys <- flushTQueue schedulerUpsweepQueue
        _ <- foldM (prepareToRunKeyCached db) mempty dirtys
        return dirtys

prepareToRunKeysRealTime :: Database -> IO ()
prepareToRunKeysRealTime db@Database{..} = do
    -- pop one at a time to reduce fraction
    atomically $ do
        let SchedulerState{..} = databaseScheduler
        dbNotLocked db
        enque <- readTQueue schedulerUpsweepQueue
        prepareToRunKey enque db
    prepareToRunKeysRealTime db

-- decrease the pending count of a key in databaseRunningPending
-- if the pending count reaches zero, we move it to databaseRunningReady and remove it from databaseRunningPending
decreasePendingCount :: Key -> Result -> Database -> STM ()
decreasePendingCount k res db@Database{..} = do
    let SchedulerState{..} = databaseScheduler
    mCount <- SMap.lookup k schedulerRunningPending
    case mCount of
        Nothing -> pure ()
        Just (c, runMode, mRes)
          | c <= 1 -> do
                -- Done waiting: move to ready and remove from pending
                SMap.delete k schedulerRunningPending
                prio <- lookupDatabaseRuntimeDepRootCounter k db
                TPQ.writeTPQueue schedulerRunningReady prio (k, newRunMode, mRes)
          | otherwise ->
                -- Decrement pending count
                SMap.insert (c - 1, newRunMode, mRes) k schedulerRunningPending
            where newRunMode = case mRes of
                    Just pRes -> runMode <> isRunDepChangedOne pRes res
                    Nothing   -> runMode


-- When a key becomes clean, decrement pending counters of its reverse dependents
-- gathered from both runtime and stored reverse maps
-- and remove it from runnning dirties and blocked sets
-- todo cleanhook once runnning is begin
cleanHook :: Key -> Database -> STM ()
cleanHook k db = do
    -- remove itself from running dirties and blocked sets
    let SchedulerState{..} = databaseScheduler db
    SSet.delete k schedulerAllDirties

-- When a key becomes clean, decrement pending counters of its reverse dependents
-- gathered from both runtime and stored reverse maps.
decreaseMyReverseDepsPendingCount :: Key -> Result -> Database -> STM ()
decreaseMyReverseDepsPendingCount k res db@Database{..} = do
    -- Gather reverse dependents from runtime map and stored reverse deps
    -- mStored  <- SMap.lookup k databaseValues
    mRuntime <- SMap.lookup k databaseRRuntimeDep
    let
        -- rdepsStored  = maybe mempty keyReverseDeps mStored
        rdepsRuntime = fromMaybe mempty mRuntime
        parents = deleteKeySet (newKey "root") rdepsRuntime
    -- For each parent, decrement its pending count; enqueue if it hits zero
    forM_ (toListKeySet parents) $ \p -> decreasePendingCount p res db

writeUpsweepQueue :: [Key] -> Database -> STM ()
writeUpsweepQueue ks Database{..} = do
    let SchedulerState{..} = databaseScheduler
    forM_ ks $ \k -> do
        writeTQueue schedulerUpsweepQueue k
        SSet.insert k schedulerAllDirties
    writeTVar schedulerAllKeysInOrder ks
    writeTVar schedulerAllKeysInOrderSize $ length ks


-- gather all dirty keys that is not finished, to reschedule after restart
-- includes keys in databaseDirtyTargets, databaseRunningReady, databaseRunningPending, databaseRunningDirties
-- and clears them from the database
popOutDirtykeysDB :: Database -> STM [Key]
popOutDirtykeysDB Database{..} = do
    let SchedulerState{..} = databaseScheduler
    -- 1. upsweep queue: drain all (atomic flush)
    void $ flushTQueue schedulerUpsweepQueue

    -- 2. Ready queue: drain all (atomic flush)
    void $ TPQ.flushTPQueue schedulerRunningReady

    -- 3. Pending map: collect keys and clear
    SMap.reset schedulerRunningPending

    -- 4. Running dirties set: read and clear
    -- runningDirties <- readTVar schedulerRunningDirties
    -- SSet.reset schedulerRunningDirties

    -- 5. Also clear blocked subset for consistency
    -- SSet.reset schedulerRunningBlocked

    -- 6. All dirties set: read and clear
    allKeys <- readTVar schedulerAllKeysInOrder
    _ <- writeTVar schedulerAllKeysInOrder mempty
    writeTVar schedulerAllKeysInOrderSize 0
    res <- filterM (`SSet.lookup` schedulerAllDirties) allKeys
    SSet.reset schedulerAllDirties
    return res

-- read one key from ready queue, and insert it into running dirties
-- this function will block if there is no key in ready queue
-- and also block if the number of running non-blocked keys exceeds maxThreads
-- readReadyQueue :: Database -> STM Key
readReadyQueue :: Database -> STM (Key, RunMode, Maybe Result)
readReadyQueue db@Database{..} = do
    dbNotLocked db
    let SchedulerState{..} = databaseScheduler
    TPQ.readTPQueue schedulerRunningReady

