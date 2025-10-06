-- We deliberately want to ensure the function we add to the rule database
-- has the constraints we need on it when we get it out.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE CPP                #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

module Development.IDE.Graph.Internal.Database (compute, newDatabase, incDatabase, build, getDirtySet, getKeysAndVisitAge, AsyncParentKill(..), computeToPreserve, transitiveDirtyListBottomUp, getRunTimeRDeps, spawnAsyncWithDbRegistration, upsweepAction, incDatabase1) where

import           Prelude                                  hiding (unzip)

import           Control.Concurrent.STM.Stats             (STM, atomicallyNamed,
                                                           modifyTVar,
                                                           modifyTVar',
                                                           newTQueueIO,
                                                           newTVarIO, readTVar,
                                                           readTVarIO, retry,
                                                           writeTVar)
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class                   (MonadIO (liftIO))
import qualified Control.Monad.RWS                        as RWS
import           Control.Monad.Trans.Class                (lift)
import           Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Strict         as State
import           Data.Dynamic
import           Data.Foldable                            (traverse_)
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
import           UnliftIO                                 (Async, MVar,
                                                           atomically,
                                                           isAsyncException,
                                                           newEmptyMVar,
                                                           newTVar, putMVar,
                                                           readMVar)

import           Data.Either                              (partitionEithers)
import           Development.IDE.Graph.Internal.Scheduler (cleanHook,
                                                           decreaseMyReverseDepsPendingCount,
                                                           insertBlockedKey,
                                                           popOutDirtykeysDB,
                                                           readReadyQueue,
                                                           writeUpsweepQueue)
import qualified StmContainers.Set                        as SSet

#if MIN_VERSION_base(4,19,0)
import           Data.Functor                             (unzip)
#else
import           Data.List.NonEmpty                       (unzip)
#endif


newDatabase :: (String -> IO ()) ->  DBQue -> ActionQueue -> Dynamic -> TheRules -> IO Database
newDatabase dataBaseLogger databaseQueue databaseActionQueue databaseExtra databaseRules = do
    databaseStep <- newTVarIO $ Step 0
    databaseThreads <- newTVarIO []
    databaseValuesLock <- newTVarIO True
    databaseValues <- atomically SMap.new
    databaseRRuntimeDep <- atomically SMap.new
    databaseRuntimeDepRoot <- atomically SMap.new
    -- Initialize scheduler state
    schedulerRunningDirties <- SSet.newIO
    schedulerRunningBlocked  <- SSet.newIO
    schedulerRunningReady    <- newTQueueIO
    schedulerRunningPending <- atomically SMap.new
    schedulerUpsweepQueue <- newTQueueIO
    schedulerAllDirties <- newTVarIO mempty
    schedulerAllKeysInOrder <- newTVarIO []
    let databaseScheduler = SchedulerState{..}
    pure Database{..}

-- incDatabase1 :: Database -> Maybe (KeySet, KeySet) -> IO [Key]
incDatabase1 :: Database -> Maybe (([Key], [Key]), KeySet) -> IO KeySet
incDatabase1 db (Just (kk, preserves)) = incDatabase db (Just (kk, preserves ))
incDatabase1 db Nothing                = incDatabase db Nothing

-- | Increment the step and mark dirty.
--   Assumes that the database is not running a build
-- only some keys are dirty
incDatabase :: Database -> Maybe (([Key], [Key]), KeySet) -> IO KeySet
incDatabase db (Just ((oldkeys, newKeys), preserves)) = do
    atomicallyNamed "incDatabase" $ modifyTVar'  (databaseStep db) $ \(Step i) -> Step $ i + 1
    forM_ newKeys $ \newKey -> atomically $ SMap.focus updateDirty newKey (databaseValues db)
    atomically $ writeUpsweepQueue (filter (not . isRootKey) $ oldkeys ++ newKeys) db
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
computeToPreserve :: Database -> KeySet -> STM ([(DeliverStatus, Async ())], ([Key], [Key]), Int)
computeToPreserve db dirtySet = do
  -- All keys that depend (directly or transitively) on any dirty key
  oldUpSweepDirties <- popOutDirtykeysDB db
  (oldKeys, newKeys, affected) <- transitiveDirtyListBottomUpDiff db (toListKeySet dirtySet) oldUpSweepDirties
  threads <- readTVar $ databaseThreads db
  let isNonAffected (k, _async) = (deliverKey k) /= newKey "root" && (deliverKey k) `notMemberKeySet` affected
  let unaffected = filter isNonAffected $ threads
  pure (unaffected, (oldKeys, newKeys), length newKeys)

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
builderOne' firstTime parentKey db@Database {..} stack key = do
  traceEvent ("builderOne: " ++ show key) return ()
  barrier <- newEmptyMVar
  -- join is used to register the async
  join $ atomicallyNamed "builder" $ do
    -- Spawn the id if needed
    case firstTime of
        FirstTime -> do
            dbNotLocked db
            insertdatabaseRuntimeDep key parentKey db
        NotFirstTime -> return ()
    status <- SMap.lookup key databaseValues
    current <- readTVar databaseStep

    case (viewToRun current . keyStatus) =<< status of
      Nothing -> do
        insertBlockedKey parentKey key db
        SMap.focus (updateStatus $ Running current Nothing barrier) key databaseValues
        let register = spawnRefresh db stack key barrier Nothing refresh
                        $ atomicallyNamed "builderOne rollback" $ SMap.delete key databaseValues
        return $ register >> return (BCContinue $ readMVar barrier)
      Just (Dirty _) -> do
        insertBlockedKey parentKey key db
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
        | otherwise -> do
            insertBlockedKey parentKey key db
            pure . pure $ BCContinue $ readMVar wait

-- Original spawnRefresh implementation moved below to use the abstraction
handleResult :: (Show a1, MonadIO m) => a1 -> MVar (Either a2 (a1, b)) -> Either a2 b -> m ()
handleResult k barrier eResult = do
    case eResult of
        Right r -> putMVar barrier (Right (k, r))
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

-- When an change event happens,
-- we mark transitively all the keys that depend on the changed key as dirty.
-- then when we upsweep, we just fire and set it as clean

-- the same event or new event might reach the same key multiple times,
-- but we only need to process it once.
-- so when upsweep, we keep a eventStep, when the eventStep is older than the newest visit step of the key
-- we just stop the key and stop propogating further.

-- if we allow downsweep, it might see two diffrent state of the same key by peeking at
-- a key the event have not reached yet, and a key the event have reached.
-- this might cause inconsistency.
-- so we simply wait for the upsweep to finish before allowing to peek at the key.
-- But if it is not there at all, we compute it. Since upsweep only propogate when a key changed,

-- a version of upsweep that only freshes the key in order and use semophore to limit the concurrency
-- it is simpler and should be more efficient in the case of many keys to upsweep
upsweep1 :: Database -> Stack -> IO ()
upsweep1 db stack = go
    where
        go = do
            k <- atomically $ readReadyQueue db
            upsweep db stack k
            go

upsweepAction :: Action ()
upsweepAction = Action $ do
    SAction{..} <- RWS.ask
    let db = actionDatabase
    liftIO $ upsweep1 db actionStack

-- do
upsweep :: Database -> Stack -> Key -> IO ()
upsweep db@Database {..} stack key = mask $ \restore -> do
  barrier <- newEmptyMVar
  join $ atomicallyNamed "upsweep" $ do
    dbNotLocked db
    -- insertdatabaseRuntimeDep childtKey key db
    status <- SMap.lookup key databaseValues
    current <- readTVar databaseStep
    case viewDirty current $ maybe (Dirty Nothing) keyStatus status of
      -- if it is still dirty, we update it and propogate further
      (Dirty s) -> do
        SMap.focus (updateStatus $ Running current s barrier) key databaseValues
        -- if it is clean, other event update it, so it is fine.
        return $ do
            spawnRefresh db stack key barrier s (\db stack key s -> restore $ do
                result <- refresh db stack key s
                atomically $ cleanHook key db
                return result) $ atomicallyNamed "upsweep rollback" $ SMap.focus updateDirty key databaseValues
      _ -> return $ atomically $ cleanHook key db

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
    -- todo, it does not consider preserving, since a refresh is not added to deps
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
    return $ (deleteKeySet (newKey "root") <$> r)



-- Legacy helper (no longer used): compute transitive dirty set
-- transitiveDirtySet :: Foldable t => Database -> t Key -> IO KeySet
-- transitiveDirtySet database = flip State.execStateT mempty . traverse_ loop
--   where
--     loop x = do
--         seen <- State.get
--         if x `memberKeySet` seen then pure () else do
--             State.put (insertKeySet x seen)
--             next <- lift $ atomically $ getReverseDependencies database x
--             traverse_ loop (maybe mempty toListKeySet next)

-- | A variant of 'transitiveDirtySet' that returns the affected keys
-- in a bottom-up dependency order (children before parents).
--
-- Edges in the reverse-dependency graph go from a child to its parents.
-- We perform a DFS and, after exploring all outgoing edges, cons the node onto
-- the accumulator. This yields children-before-parents order directly.
transitiveDirtyListBottomUp :: Database -> [Key] -> IO [Key]
transitiveDirtyListBottomUp database seeds = do
  acc <- newIORef ([] :: [Key])
  let go x = do
        seen <- State.get
        if x `memberKeySet` seen
          then pure ()
          else do
            State.put (insertKeySet x seen)
            when (not (isRootKey x)) $ do
                mnext <- lift $ atomically $ getRunTimeRDeps database x
                traverse_ go (maybe mempty toListKeySet mnext)
            lift $ modifyIORef' acc (x :)
  -- traverse all seeds
  void $ State.runStateT (traverse_ go seeds) mempty
  readIORef acc

-- the lefts are keys that are no longer affected, we can try to mark them clean
-- the rights are new affected keys, we need to mark them dirty
transitiveDirtyListBottomUpDiff :: Foldable t => Database -> t Key -> [Key] -> STM ([Key], [Key], KeySet)
transitiveDirtyListBottomUpDiff database seeds allOldKeys = do
  acc <- newTVar []
  let go1 x = do
        seen <- State.get
        if x `memberKeySet` seen
          then pure ()
          else do
            State.put (insertKeySet x seen)
            when (not (isRootKey x)) $ do
                mnext <- lift $ getRunTimeRDeps database x
                traverse_ go1 (maybe mempty toListKeySet mnext)
            lift $ modifyTVar acc (x :)
  -- traverse all seeds
  seen <- snd <$> State.runStateT (do traverse_ go1 seeds) mempty
  newKeys <- readTVar acc
  let oldKeys = filter (`notMemberKeySet` seen) allOldKeys
  return (oldKeys, newKeys, seen)


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
  IO () ->
  IO ()
spawnRefresh db@Database {..} stack key barrier prevResult refresher rollBack = do
  Step currentStep <- atomically $ readTVar databaseStep
  spawnAsyncWithDbRegistration
    db
    (return $ DeliverStatus currentStep ("async computation; " ++ show key) key)
    (refresher db stack key prevResult)
    (\r -> do
        case r of
            Left e  -> when (isAsyncException e) rollBack --- IGNORE ---
            Right _ -> return ()
        handleResult key barrier r
    )

-- Attempt to clear a Dirty parent that ended up with unchanged children during this event.
-- If the parent is Dirty, and every direct child is either Clean/Exception/Running for a step < eventStep,
-- and no child changed at/after eventStep, mark parent Clean (preserving its last Clean result),
-- and recursively attempt the same for its own parents.



