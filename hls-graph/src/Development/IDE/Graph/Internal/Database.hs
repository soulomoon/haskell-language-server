-- We deliberately want to ensure the function we add to the rule database
-- has the constraints we need on it when we get it out.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE CPP                #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

module Development.IDE.Graph.Internal.Database (compute, newDatabase, incDatabase, build, getDirtySet, getKeysAndVisitAge, AsyncParentKill(..), computeToPreserve, getRunTimeRDeps, spawnAsyncWithDbRegistration, upsweepAction) where

import           Prelude                                  hiding (unzip)

import           Control.Concurrent.STM.Stats             (STM, atomicallyNamed,
                                                           modifyTVar',
                                                           newTQueueIO,
                                                           newTVarIO, readTVar,
                                                           readTVarIO, retry)
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class                   (MonadIO (liftIO))
import qualified Control.Monad.RWS                        as RWS
import           Control.Monad.Trans.Reader
import           Data.Dynamic
import           Data.Foldable                            (foldrM)
import           Data.IORef.Extra
import           Data.Maybe
import           Data.Traversable                         (for)
import           Data.Tuple.Extra
import           Debug.Trace                              (traceEvent)
import           Development.IDE.Graph.Classes
import           Development.IDE.Graph.Internal.Key
import           Development.IDE.Graph.Internal.Rules
import           Development.IDE.Graph.Internal.Types
import           Development.IDE.Graph.Internal.Types     ()
import           Development.IDE.WorkerThread             (DeliverStatus (..))
import qualified Focus
import qualified ListT
import qualified StmContainers.Map                        as SMap
import           System.Time.Extra                        (duration)
import           UnliftIO                                 (MVar, atomically,
                                                           isAsyncException,
                                                           newEmptyMVar,
                                                           putMVar, readMVar)

import           Development.IDE.Graph.Internal.Scheduler (cleanHook,
                                                           decreaseMyReverseDepsPendingCount,
                                                           popOutDirtykeysDB,
                                                           readReadyQueue,
                                                           writeUpsweepQueue)
import qualified UnliftIO.Exception                       as UE

#if MIN_VERSION_base(4,19,0)
import           Data.Functor                             (unzip)
#else
import           Data.List.NonEmpty                       (unzip)
#endif


newDatabase :: (String -> IO ()) -> DBQue -> ActionQueue -> Dynamic -> TheRules -> IO Database
newDatabase dataBaseLogger databaseQueue databaseActionQueue databaseExtra databaseRules = do
    databaseStep <- newTVarIO $ Step 0
    databaseThreads <- newTVarIO []
    databaseValuesLock <- newTVarIO True
    databaseValues <- atomically SMap.new
    databaseRRuntimeDep <- atomically SMap.new
    databaseRuntimeDepRoot <- atomically SMap.new
    databaseRRuntimeDepRoot <- atomically SMap.new
    databaseTransitiveRRuntimeDepCache <- atomically SMap.new
    -- Initialize scheduler state
    schedulerRunningReady    <- newTQueueIO
    schedulerRunningPending <- atomically SMap.new
    schedulerUpsweepQueue <- newTQueueIO
    schedulerAllDirties <- newTVarIO mempty
    schedulerAllKeysInOrder <- newTVarIO []
    let databaseScheduler = SchedulerState{..}
    pure Database{..}

-- | Increment the step and mark dirty.
--   Assumes that the database is not running a build
-- only some keys are dirty
incDatabase :: Database -> Maybe (([Key], [Key]), KeySet) -> IO KeySet
incDatabase db (Just ((oldkeys, newKeys), preserves)) = do
    atomicallyNamed "incDatabase" $ modifyTVar' (databaseStep db) $ \(Step i) -> Step $ i + 1
    forM_ newKeys $ \newKey -> atomically $ SMap.focus updateDirty newKey (databaseValues db)
    -- only upsweep the keys that are not preserved
    atomically $ writeUpsweepQueue (filter  (`notMemberKeySet` preserves) oldkeys ++ newKeys) db
    return $ preserves

-- all keys are dirty
incDatabase db Nothing = do
    atomically $ modifyTVar'  (databaseStep db) $ \(Step i) -> Step $ i + 1
    let list = SMap.listT (databaseValues db)
    -- all running keys are also dirty
    atomicallyNamed "incDatabase - all " $ flip ListT.traverse_ list $ \(k,_) ->
        SMap.focus updateDirty k (databaseValues db)
    return $ mempty

-- computeToPreserve :: Database -> KeySet -> STM ([(DeliverStatus, Async ())], ([Key], [Key]))
-- computeToPreserve :: Database -> KeySet -> STM ([(DeliverStatus, Async ())], ([Key], [Key]), Int)
-- computeToPreserve :: Database -> KeySet -> STM (KeySet, ([Key], [Key]), Int)
computeToPreserve :: Database -> KeySet -> STM (KeySet, ([Key], [Key]), Int, [Key])
computeToPreserve db dirtySet = do
  -- All keys that depend (directly or transitively) on any dirty key
--   traceEvent ("markDirty base " ++ show dirtySet) $ return ()
  oldUpSweepDirties <- popOutDirtykeysDB db
  (oldKeys, newKeys, affected) <- transitiveDirtyListBottomUpDiff db (toListKeySet dirtySet) oldUpSweepDirties
--   traceEvent ("oldKeys " ++ show oldKeys) $ return ()
--   traceEvent ("newKeys " ++ show newKeys) $ return ()
  pure (affected, (oldKeys, newKeys), length newKeys, oldUpSweepDirties)

updateDirty :: Monad m => Focus.Focus KeyDetails m ()
updateDirty = Focus.adjust $ \(KeyDetails status rdeps) ->
            let status'
                  | Running _ x _ <- status = Dirty x
                  | Clean x <- status = Dirty (Just x)
                  | otherwise = status
            in KeyDetails status' rdeps


-- updateClean :: Monad m => Focus.Focus KeyDetails m ()
-- updateClean = Focus.adjust $ \(KeyDetails _ rdeps) ->
-- | Unwrap and build a list of keys in parallel
build ::
  forall f key value.
  (Traversable f, RuleResult key ~ value, Typeable key, Show key, Hashable key, Eq key, Typeable value) =>
  Key -> Database -> Stack -> f key -> IO (f Key, f value)
-- build _ st k | traceShow ("build", st, k) False = undefined
build pk db stack keys = do
  built <- builder pk db stack (fmap newKey keys)
  let (ids, vs) = unzip built
  pure (ids, fmap (asV . resultValue) vs)
  where
    asV :: Value -> value
    asV (Value x) = unwrapDynamic x


-- | Build a list of keys and return their results.
--  If none of the keys are dirty, we can return the results immediately.
--  Otherwise, a blocking computation is returned *which must be evaluated asynchronously* to avoid deadlock.
builder :: (Traversable f) => Key -> Database -> Stack -> f Key -> IO (f (Key, Result))
-- builder _ st kk | traceShow ("builder", st,kk) False = undefined
builder pk db stack keys = do
    waits <- for keys (\k -> builderOne pk db stack k)
    for waits (interpreBuildContinue db pk)

-- the first run should not block
data BuildContinue = BCContinue (IO (Either SomeException (Key, Result))) | BCStop Key Result

-- interpreBuildContinue :: BuildContinue -> IO (Key, Result)
interpreBuildContinue :: Database -> Key -> (Key, BuildContinue) -> IO (Key, Result)
interpreBuildContinue _db _pk (_kid, BCStop k v)     = return (k, v)
interpreBuildContinue _db _pk (_kid, BCContinue ioR) = do
    r <- ioR
    case r of
        Right kv -> return kv
        Left e   -> throw e

builderOne :: Key -> Database -> Stack -> Key -> IO (Key, BuildContinue)
builderOne parentKey db stack kid = do
    r <- builderOne' FirstTime parentKey db stack kid
    return (kid, r)

data FirstTime = FirstTime | NotFirstTime

builderOne' :: FirstTime -> Key -> Database -> Stack -> Key -> IO BuildContinue
builderOne' firstTime parentKey db@Database {..} stack key = UE.uninterruptibleMask $ \restore -> do
  traceEvent ("builderOne: " ++ show key) return ()
  barrier <- newEmptyMVar
  -- join is used to register the async
  join $ restore $ atomicallyNamed "builder" $ do
    dbNotLocked db
    -- Spawn the id if needed
    case firstTime of
        FirstTime -> do
            insertdatabaseRuntimeDep key parentKey db
        NotFirstTime -> return ()
    status <- SMap.lookup key databaseValues
    current <- readTVar databaseStep

    case (viewToRun current . keyStatus) =<< status of
      Nothing -> do
        SMap.focus (updateStatus $ Running current Nothing barrier) key databaseValues
        let register = spawnRefresh db stack key barrier Nothing refresh
                        -- why it is important to use rollback here

                        {- Note [Rollback is required if killed before registration]
                        It is important to use rollback here because a key might be killed before it is registered, even though it is not one of the dirty keys.
                        In this case, it would skip being marked as dirty. Therefore, we have to roll back here if it is killed, to ensure consistency.
                        -}
                        (\_ -> atomicallyNamed "builderOne rollback" $ SMap.delete key databaseValues)
                        restore
        return $ register >> return (BCContinue $ readMVar barrier)
      Just (Dirty _) -> do
        case firstTime of
            FirstTime -> pure . pure $ BCContinue $ do
                    br <- builderOne' NotFirstTime parentKey db stack key
                    case br of
                        BCContinue ioR -> ioR
                        BCStop k r     -> pure $ Right (k, r)
            NotFirstTime -> retry
      Just (Clean r) -> pure . pure $ BCStop key r
      Just (Running _step _s wait)
        | memberStack key stack -> throw $ StackException stack
        | otherwise -> pure . pure $ BCContinue $ readMVar wait

-- Original spawnRefresh implementation moved below to use the abstraction
-- handleResult :: (Show a1, MonadIO m) => a1 -> MVar (Either a2 (a1, b)) -> Either a2 b -> m ()
handleResult :: MonadIO m => Key -> MVar (Either SomeException (Key, b)) -> Either SomeException b -> m ()
handleResult k barrier eResult = do
    case eResult of
        Right r -> putMVar barrier (Right (k, r))
        -- accumulate the async kill info for debugging
        Left e | Just (AsyncParentKill tid s ks) <- fromException e  -> putMVar barrier (Left (toException $ AsyncParentKill tid s (k:ks)))
        Left e  -> putMVar barrier (Left e)

-- | isDirty
-- only dirty when it's build time is older than the changed time of one of its dependencies
isDirty :: Foldable t => Result -> t (a, Result) -> Bool
isDirty me = any (\(_,dep) -> resultBuilt me < resultChanged dep)

-- | Refresh dependencies for a key and compute the key:
-- The refresh the deps linearly(last computed order of the deps for the key).
-- If any of the deps is dirty in the process, we jump to the actual computation of the key
-- and shortcut the refreshing of the rest of the deps.
-- * If no dirty dependencies and we have evaluated the key previously, then we refresh it in the current thread.
--   This assumes that the implementation will be a lookup
-- * Otherwise, we spawn a new thread to refresh the dirty deps (if any) and the key itself
refreshDeps :: KeySet -> Database -> Stack -> Key -> Result -> [KeySet] -> IO Result
refreshDeps visited db stack key result = \case
    -- no more deps to refresh
    [] -> compute db stack key RunDependenciesSame (Just result)
    (dep:deps) -> do
        let newVisited = dep <> visited
        res <- builder key db stack (toListKeySet (dep `differenceKeySet` visited))
        if isDirty result res
                -- restart the computation if any of the deps are dirty
                then compute db stack key RunDependenciesChanged (Just result)
                -- else kick the rest of the deps
                else refreshDeps newVisited db stack key result deps

-- propogate up the changes

-- When an key change event happens,
-- we mark transitively all the keys that depend on the changed key as dirty.
-- then when we upsweep, we just fire and set it as clean

-- the same event or new event might reach the same key multiple times,
-- but we only need to process it once. So we only process when it is dirty.

-- a version of upsweep that only freshes the key in topo order and limit the concurrency
-- it is simpler and should be more efficient when too many keys need to be upswept
upsweepAll :: Database -> Stack -> IO ()
upsweepAll db stack = go
    where
        go = do
            k <- atomically $ readReadyQueue db
            upsweep db stack k
            go

upsweepAction :: Action ()
upsweepAction = Action $ do
    SAction{..} <- RWS.ask
    let db = actionDatabase
    liftIO $ upsweepAll db actionStack

upsweep :: Database -> Stack -> Key -> IO ()
upsweep db@Database {..} stack key = UE.uninterruptibleMask $ \k -> do
  barrier <- newEmptyMVar
  join $ k $ atomicallyNamed "upsweep" $ do
    dbNotLocked db
    status <- SMap.lookup key databaseValues
    current <- readTVar databaseStep
    case keyStatus <$> status of
      -- if it is still dirty, we update it and propogate further
      Just (Dirty s) -> do
        SMap.focus (updateStatus $ Running current s barrier) key databaseValues
        -- if it is clean, other event update it, so it is fine.
        return $
            spawnRefresh db stack key barrier s (\db stack key s -> do
                result <- refresh db stack key s
                -- todo, maybe just put this to refresh
                -- atomically $ cleanHook key db
                return result)
                -- see Note [Rollback is required if killed before registration]
                (const $ atomicallyNamed "upsweep rollback" $ SMap.focus updateDirty key databaseValues)
                -- (traceEventIO $ "markDirty should " ++ show key)
                k
      Just (Clean _) -> return $ atomically $ cleanHook key db
    --   leave it for downsweep
      Nothing -> return $ atomically $ cleanHook key db
      _ -> return . return $ ()

-- refresh :: Database -> Stack -> Key -> Maybe Result -> IO Result
-- refresh _ st k _ | traceShow ("refresh", st, k) False = undefined
refresh :: Database -> Stack -> Key -> Maybe Result -> IO Result
refresh db stack key result = case (addStack key stack, result) of
    (Left e, _) -> throw e
    (Right stack, Just me@Result{resultDeps = ResultDeps deps}) -> refreshDeps mempty db stack key me (reverse deps)
    (Right stack, _) -> compute db stack key RunDependenciesChanged result
-- | Compute a key.
compute :: Database -> Stack -> Key -> RunMode -> Maybe Result -> IO Result
-- compute _ st k _ _ | traceShow ("compute", st, k) False = undefined
compute db@Database{..} stack key mode result = do
    let act = runRule databaseRules key (fmap resultData result) mode
    deps <- liftIO $ newIORef UnknownDeps
    curStep <- liftIO $ readTVarIO databaseStep
    dataBaseLogger $ "Computing key: " ++ show key ++ " at step " ++ show curStep
    (execution, RunResult{..}) <-
        liftIO $ duration $ runReaderT (fromAction act) $ SAction key db deps stack
    deps <- liftIO $ readIORef deps
    let lastChanged = maybe curStep resultChanged result
    let lastBuild = maybe curStep resultBuilt result
    -- changed time is always older than or equal to build time
    let (changed, built) =  case runChanged of
            -- some thing changed
            ChangedRecomputeDiff -> (curStep, curStep)
            -- recomputed is the same
            ChangedRecomputeSame -> (lastChanged, curStep)
            -- nothing changed
            ChangedNothing       -> (lastChanged, lastBuild)
    let -- only update the deps when the rule ran with changes
        actualDeps = if runChanged /= ChangedNothing then deps else previousDeps
        previousDeps= maybe UnknownDeps resultDeps result
    let res = Result { resultValue = runValue, resultBuilt = built, resultChanged = changed, resultVisited = curStep, resultDeps = actualDeps, resultExecution = execution, resultData = runStore }
    liftIO $ atomicallyNamed "compute and run hook" $ do
        dbNotLocked db
        case getResultDepsDefault mempty actualDeps of
            deps | not (nullKeySet deps)
                && runChanged /= ChangedNothing
                        -> do
                -- IMPORTANT: record the reverse deps **before** marking the key Clean.
                -- If an async exception strikes before the deps have been recorded,
                -- we won't be able to accurately propagate dirtiness for this key
                -- on the next build.
                updateReverseDeps key db
                    (getResultDepsDefault mempty previousDeps)
                    deps
            _ -> pure ()
        runHook
        decreaseMyReverseDepsPendingCount key db
        cleanHook key db
        -- todo
        -- it might be overridden by error if another kills this thread
        SMap.focus (updateStatus $ Clean res) key databaseValues
    pure res

updateStatus :: Monad m => Status -> Focus.Focus KeyDetails m ()
updateStatus res = Focus.alter
    (Just . maybe (KeyDetails res mempty)
    (\it -> it{keyStatus = res}))

-- | Returns the set of dirty keys annotated with their age (in # of builds)
getDirtySet :: Database -> IO [(Key, Int)]
getDirtySet db = do
    Step curr <- readTVarIO (databaseStep db)
    dbContents <- getDatabaseValues db
    let calcAge Result{resultBuilt = Step x} = curr - x
        calcAgeStatus (Dirty x)=calcAge <$> x
        calcAgeStatus _         = Nothing
    return $ mapMaybe (secondM calcAgeStatus) dbContents

-- | Returns an approximation of the database keys,
-- | make a change on most of the thinkgs is good
--   annotated with how long ago (in # builds) they were visited
getKeysAndVisitAge :: Database -> IO [(Key, Int)]
getKeysAndVisitAge db = do
    values <- getDatabaseValues db
    Step curr <- readTVarIO (databaseStep db)
    let keysWithVisitAge = mapMaybe (secondM (fmap getAge . getResult)) values
        getAge Result{resultVisited = Step s} = curr - s
    return keysWithVisitAge
--------------------------------------------------------------------------------
-- Reverse dependencies

-- | Update the reverse dependencies of an Id
updateReverseDeps
    :: Key        -- ^ Id
    -> Database
    -> KeySet -- ^ Previous direct dependencies of Id
    -> KeySet -- ^ Current direct dependencies of Id
    -> STM ()
-- mask to ensure that all the reverse dependencies are updated
updateReverseDeps myId db prev new = do
    forM_ (toListKeySet $ prev `differenceKeySet` new) $ \d ->
         doOne (deleteKeySet myId) d
    forM_ (toListKeySet new) $
        doOne (insertKeySet myId)
    where
        alterRDeps f =
            Focus.adjust (onKeyReverseDeps f)
        -- updating all the reverse deps atomically is not needed.
        -- Therefore, run individual transactions for each update
        -- in order to avoid contention
        doOne f id = SMap.focus (alterRDeps f) id (databaseValues db)

-- compute the transitive reverse dependencies of a set of keys

-- non-root
-- inline
{-# INLINE getRunTimeRDeps #-}
getRunTimeRDeps :: Database -> Key -> STM (Maybe KeySet)
getRunTimeRDeps db k = do
    r <- SMap.lookup k (databaseRRuntimeDep db)
    return (deleteKeySet (newKey "root") <$> r)

-- Edges in the reverse-dependency graph go from a child to its parents.
-- We perform a DFS and, after exploring all outgoing edges, cons the node onto
-- the accumulator. This yields children-before-parents order directly.

-- the lefts are keys that are no longer affected, we can try to mark them clean
-- the rights are new affected keys, we need to mark them dirty
transitiveDirtyListBottomUpDiff :: Database -> [Key] -> [Key] -> STM ([Key], [Key], KeySet)
transitiveDirtyListBottomUpDiff database seeds allOldKeys = do
  (newKeys, seen) <- cacheTransitiveDirtyListBottomUpDFSWithRootKey database $ fromListKeySet seeds
  let oldKeys = filter (`notMemberKeySet` seen) allOldKeys
  return (oldKeys, newKeys, seen)

cacheTransitiveDirtyListBottomUpDFSWithRootKey :: Database -> KeySet -> STM ([Key], KeySet)
cacheTransitiveDirtyListBottomUpDFSWithRootKey db@Database{..} seeds = do
  (newKeys, seen) <- cacheTransitiveDirtyListBottomUpDFS db seeds
  --   we should put pump root keys back to seen
--   for each new key, get its root keys and put them back to seen
  foldrM (\k acc -> do
            mroot <- SMap.lookup k databaseRRuntimeDepRoot
            case mroot of
                Just roots -> return $ foldr insertKeySet acc (toListKeySet roots)
                Nothing    -> return acc
        ) seen newKeys >>= \seen' -> return (newKeys, seen')



cacheTransitiveDirtyListBottomUpDFS :: Database -> KeySet -> STM ([Key], KeySet)
cacheTransitiveDirtyListBottomUpDFS db@Database{..} seeds = do
    SMap.lookup seeds databaseTransitiveRRuntimeDepCache >>= \case
        Just v  -> return v
        Nothing -> do
            r <- transitiveDirtyListBottomUpDFS db seeds
            SMap.insert r seeds databaseTransitiveRRuntimeDepCache
            return r

transitiveDirtyListBottomUpDFS :: Database -> KeySet -> STM ([Key], KeySet)
transitiveDirtyListBottomUpDFS database seeds = do
  let go1 :: Key -> ([Key], KeySet) -> STM ([Key], KeySet)
      go1 x acc@(dirties, seen) = do
        if x `memberKeySet` seen
          then pure acc
          else do
            let newAcc = (dirties, insertKeySet x seen)
            mnext <- getRunTimeRDeps database x
            (newDirties, newSeen) <- foldrM go1 newAcc (maybe mempty toListKeySet mnext)
            return (x:newDirties, newSeen)
                -- if it is root key, we do not add it to the dirty list
                -- since root key is not up for upsweep
                -- but it would be in the seen list, so we would kill dirty root key async
  -- traverse all seeds
  foldrM go1 ([], mempty) (toListKeySet seeds)

-- | Original spawnRefresh using the general pattern
-- inline
{-# INLINE spawnRefresh #-}
spawnRefresh ::
  Database ->
  t ->
  Key ->
  MVar (Either SomeException (Key, Result)) ->
  Maybe Result ->
  (Database -> t -> Key -> Maybe Result -> IO Result) ->
  (SomeException -> IO ()) ->
  (forall a. IO a -> IO a) ->
  IO ()
spawnRefresh db@Database {..} stack key barrier prevResult refresher rollBack restore = do
  Step currentStep <- atomically $ readTVar databaseStep
  spawnAsyncWithDbRegistration
    db
    (DeliverStatus currentStep ("async computation; " ++ show key) key)
    (refresher db stack key prevResult)
    (\r -> do
        case r of
            Left e  -> when (isAsyncException e) (rollBack e) --- IGNORE ---
            Right _ -> return ()
        handleResult key barrier r
    ) restore

-- Attempt to clear a Dirty parent that ended up with unchanged children during this event.
-- If the parent is Dirty, and every direct child is either Clean/Exception/Running for a step < eventStep,
-- and no child changed at/after eventStep, mark parent Clean (preserving its last Clean result),
-- and recursively attempt the same for its own parents.



