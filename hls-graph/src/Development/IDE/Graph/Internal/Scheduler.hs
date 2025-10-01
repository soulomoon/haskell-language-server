{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

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
  ) where

import           Control.Concurrent.STM               (STM, atomically, check,
                                                       flushTQueue, modifyTVar,
                                                       readTQueue, readTVar,
                                                       writeTQueue, writeTVar)
import           Control.Monad                        (forM, forM_)
import           Data.Maybe                           (fromMaybe)
import qualified ListT
import qualified StmContainers.Map                    as SMap

import           Development.IDE.Graph.Internal.Key   (Key, KeySet,
                                                       deleteKeySet,
                                                       fromListKeySet,
                                                       insertKeySet,
                                                       lengthKeySet,
                                                       memberKeySet, newKey,
                                                       toListKeySet,
                                                       unionKeySet)
import           Development.IDE.Graph.Internal.Types (Database (..),
                                                       KeyDetails (..),
                                                       Result (..), Status (..),
                                                       getResult,
                                                       getResultDepsDefault)

-- simply put the key into ready queue, without checking deps
prepareToRunKeyNative :: Key -> Database -> STM ()
prepareToRunKeyNative k Database{..} = do
    writeTQueue databaseRunningReady k
-- prepare to run a key in databaseDirtyTargets
-- we first peek if all the deps are clean
-- if so, we insert it into databaseRunningReady
-- otherwise, we insert it into databaseRunningPending with the pending count(the number of deps not clean)
-- so when a dep is cleaned, we can decrement the pending count, and when it reaches zero, we can move it to databaseRunningReady
prepareToRunKey :: Key -> Database -> STM ()
prepareToRunKey k Database{..} = do
    -- Determine the last known direct dependencies of k from its stored Result
    mKd <- SMap.lookup k databaseValues
    let deps = case mKd of
                Nothing -> mempty
                Just KeyDetails{keyStatus = st} ->
                    let mRes = getResult st
                    in maybe mempty (getResultDepsDefault mempty . resultDeps) mRes
        depList = filter (/= k) (toListKeySet deps)

    -- Peek dependency statuses to see how many are not yet clean
    depStatuses <- forM depList $ \d -> SMap.lookup d databaseValues
    let isCleanDep = \case
            Just KeyDetails{keyStatus = Clean _} -> True
            _                                    -> False
        pendingCount = length (filter (not . isCleanDep) depStatuses)

    if pendingCount == 0
        then do
            writeTQueue databaseRunningReady k
            SMap.delete k databaseRunningPending
        else do
            SMap.insert pendingCount k databaseRunningPending


-- only insert blocked key into databaseRunningBlocked if it is already running
insertBlockedKey :: Key -> Database -> STM ()
insertBlockedKey k Database{..} = do
    runnings <- readTVar databaseRunningDirties
    if k `memberKeySet` runnings
      then do
        blockedSet <- readTVar databaseRunningBlocked
        writeTVar databaseRunningBlocked $ insertKeySet k blockedSet
      else
        return ()

-- take out all databaseDirtyTargets and prepare them to run
prepareToRunKeys :: Foldable t => Database -> t Key -> IO ()
prepareToRunKeys db dirtys = do
    forM_ dirtys $ \k -> atomically $ prepareToRunKey k db

prepareToRunKeysRealTime :: Database -> IO ()
prepareToRunKeysRealTime db@Database{..} = do
    -- pop one at a time to reduce fraction
    atomically $ do
        enque <- readTQueue databaseUpsweepQueue
        prepareToRunKey enque db
    prepareToRunKeysRealTime db



-- decrease the pending count of a key in databaseRunningPending
-- if the pending count reaches zero, we move it to databaseRunningReady and remove it from databaseRunningPending
decreasePendingCount :: Key -> Database -> STM ()
decreasePendingCount k Database{..} = do
    mCount <- SMap.lookup k databaseRunningPending
    case mCount of
        Nothing -> pure ()
        Just c
          | c <= 1 -> do
                -- Done waiting: move to ready and remove from pending
                SMap.delete k databaseRunningPending
                writeTQueue databaseRunningReady k
          | otherwise ->
                -- Decrement pending count
                SMap.insert (c - 1) k databaseRunningPending

-- When a key becomes clean, decrement pending counters of its reverse dependents
-- gathered from both runtime and stored reverse maps
-- and remove it from runnning dirties and blocked sets
cleanHook :: Key -> Database -> STM ()
cleanHook k db = do
    decreaseMyReverseDepsPendingCount k db
    -- remove itself from running dirties and blocked sets
    runningSet <- readTVar (databaseRunningDirties db)
    writeTVar (databaseRunningDirties db) $ deleteKeySet k runningSet
    blockedSet <- readTVar (databaseRunningBlocked db)
    writeTVar (databaseRunningBlocked db) $ deleteKeySet k blockedSet

-- When a key becomes clean, decrement pending counters of its reverse dependents
-- gathered from both runtime and stored reverse maps.
decreaseMyReverseDepsPendingCount :: Key -> Database -> STM ()
decreaseMyReverseDepsPendingCount k db@Database{..} = do
    -- Gather reverse dependents from runtime map and stored reverse deps
    mRuntime <- SMap.lookup k databaseRRuntimeDep
    mStored  <- SMap.lookup k databaseValues
    let rdepsStored  = maybe mempty keyReverseDeps mStored
        rdepsRuntime = fromMaybe mempty mRuntime
        parents = deleteKeySet (newKey "root") (rdepsStored <> rdepsRuntime)
    -- For each parent, decrement its pending count; enqueue if it hits zero
    forM_ (toListKeySet parents) $ \p -> decreasePendingCount p db

writeUpsweepQueue :: [Key] -> Database -> STM ()
writeUpsweepQueue ks Database{..} = do
    forM_ ks $ \k -> writeTQueue databaseUpsweepQueue k

-- gather all dirty keys that is not finished, to reschedule after restart
-- includes keys in databaseDirtyTargets, databaseRunningReady, databaseRunningPending, databaseRunningDirties
-- and clears them from the database
popOutDirtykeysDB :: Database -> STM KeySet
popOutDirtykeysDB Database{..} = do
    -- 1. upsweep queue: drain all (atomic flush)
    toProccess <- flushTQueue databaseUpsweepQueue

    -- 2. Ready queue: drain all (atomic flush)
    readyKeys <- flushTQueue databaseRunningReady

    -- 3. Pending map: collect keys and clear
    pendingPairs <- ListT.toList (SMap.listT databaseRunningPending)
    let pendingKeys = map fst pendingPairs
    SMap.reset databaseRunningPending

    -- 4. Running dirties set: read and clear
    runningDirties <- readTVar databaseRunningDirties
    _ <- writeTVar databaseRunningDirties mempty

    -- 5. Also clear blocked subset for consistency
    _ <- writeTVar databaseRunningBlocked mempty

    -- Union all into a single KeySet to return
    let resultSet = fromListKeySet toProccess `unionKeySet` fromListKeySet readyKeys `unionKeySet` fromListKeySet pendingKeys `unionKeySet` runningDirties
    pure resultSet

-- read one key from ready queue, and insert it into running dirties
-- this function will block if there is no key in ready queue
-- and also block if the number of running non-blocked keys exceeds maxThreads
readReadyQueue :: Database -> STM Key
readReadyQueue db@Database{..} = do
    blockedOnThreadLimit db 16
    r <- readTQueue databaseRunningReady
    modifyTVar databaseRunningDirties $ insertKeySet r
    return r


computeRunningNonBlocked :: Database -> STM Int
computeRunningNonBlocked Database{..} = do
    blockedSetSize <- lengthKeySet <$> readTVar databaseRunningBlocked
    runningSetSize <- lengthKeySet <$> readTVar databaseRunningDirties
    return $ runningSetSize - blockedSetSize

blockedOnThreadLimit :: Database -> Int -> STM ()
blockedOnThreadLimit db maxThreads = do
    runningNonBlocked <- computeRunningNonBlocked db
    check $ runningNonBlocked < maxThreads


