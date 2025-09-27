{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}

module Development.IDE.Graph.Internal.Types where

import           Control.Concurrent.STM             (STM, check, modifyTVar')
import           Control.Monad                      (forM, forM_, forever,
                                                     unless, when)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.RWS                  (MonadReader (local), asks)
import           Control.Monad.Trans.Reader         (ReaderT (..))
import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.Bifunctor                     (first, second)
import qualified Data.ByteString                    as BS
import           Data.Dynamic
import           Data.Either                        (partitionEithers)
import           Data.Foldable                      (fold)
import qualified Data.HashMap.Strict                as Map
import           Data.IORef
import           Data.List                          (intercalate, partition)
import           Data.Maybe                         (fromMaybe, isJust,
                                                     isNothing)
import           Data.Set                           (Set)
import qualified Data.Set                           as S
import           Data.Typeable
import           Debug.Trace                        (traceEventIO)
import           Development.IDE.Graph.Classes
import           Development.IDE.Graph.Internal.Key
import           Development.IDE.WorkerThread       (DeliverStatus (..),
                                                     TaskQueue (..),
                                                     awaitRunInThread,
                                                     counTaskQueue,
                                                     flushTaskQueue,
                                                     writeTaskQueue)
import qualified Focus
import           GHC.Conc                           (TVar, atomically)
import           GHC.Generics                       (Generic)
import qualified ListT
import qualified StmContainers.Map                  as SMap
import           StmContainers.Map                  (Map)
import           System.Time.Extra                  (Seconds, sleep)
import           UnliftIO                           (Async (asyncThreadId),
                                                     MVar, MonadUnliftIO, async,
                                                     asyncExceptionFromException,
                                                     asyncExceptionToException,
                                                     poll, readTVar, readTVarIO,
                                                     throwTo, waitCatch,
                                                     withAsync)
import           UnliftIO.Concurrent                (ThreadId, myThreadId)
import qualified UnliftIO.Exception                 as UE

#if !MIN_VERSION_base(4,18,0)
import           Control.Applicative                (liftA2)
#endif

unwrapDynamic :: forall a . Typeable a => Dynamic -> a
unwrapDynamic x = fromMaybe (error msg) $ fromDynamic x
    where msg = "unwrapDynamic failed: Expected " ++ show (typeRep (Proxy :: Proxy a)) ++
                ", but got " ++ show (dynTypeRep x)

---------------------------------------------------------------------
-- RULES

type TheRules = Map.HashMap TypeRep Dynamic

-- | A computation that defines all the rules that form part of the computation graph.
--
-- 'Rules' has access to 'IO' through 'MonadIO'. Use of 'IO' is at your own risk: if
-- you write 'Rules' that throw exceptions, then you need to make sure to handle them
-- yourself when you run the resulting 'Rules'.
newtype Rules a = Rules (ReaderT SRules IO a)
    deriving newtype (Monad, Applicative, Functor, MonadIO)

data SRules = SRules {
    rulesExtra   :: !Dynamic,
    rulesActions :: !(IORef [Action ()]),
    rulesMap     :: !(IORef TheRules)
    }

---------------------------------------------------------------------
-- ACTIONS

-- | An action representing something that can be run as part of a 'Rule'.
--
-- 'Action's can be pure functions but also have access to 'IO' via 'MonadIO' and 'MonadUnliftIO.
-- It should be assumed that actions throw exceptions, these can be caught with
-- 'Development.IDE.Graph.Internal.Action.actionCatch'. In particular, it is
-- permissible to use the 'MonadFail' instance, which will lead to an 'IOException'.
newtype Action a = Action {fromAction :: ReaderT SAction IO a}
    deriving newtype (Monad, Applicative, Functor, MonadIO, MonadFail, MonadThrow, MonadCatch, MonadMask, MonadUnliftIO, MonadReader SAction)

runActionMonad :: Action a ->  SAction -> IO a
runActionMonad (Action r) s = runReaderT r s

data SAction = SAction {
    actionKey      :: !Key,
    actionDatabase :: !Database,
    actionDeps     :: !(IORef ResultDeps),
    actionStack    :: !Stack
    }

getDatabase :: Action Database
getDatabase = asks actionDatabase

getActionKey :: Action Key
getActionKey = asks actionKey

setActionKey :: Key -> Action a -> Action a
setActionKey k act = local (\s' -> s'{actionKey = k}) act


---------------------------------------------------------------------
-- DATABASE

data ShakeDatabase = ShakeDatabase !Int [Action ()] Database

newtype Step = Step Int
    deriving newtype (Eq,Ord,Hashable,Show,Num,Enum,Real,Integral)


getShakeStep :: MonadIO m => ShakeDatabase -> m Step
getShakeStep (ShakeDatabase _ _ db) = do
    s <- readTVarIO $ databaseStep db
    return s

lockShakeDatabaseValues :: MonadIO m => ShakeDatabase -> m ()
lockShakeDatabaseValues (ShakeDatabase _ _ db) = do
    liftIO $ atomically $ modifyTVar' (databaseValuesLock db) (const False)

unlockShakeDatabaseValues :: MonadIO m => ShakeDatabase -> m ()
unlockShakeDatabaseValues (ShakeDatabase _ _ db) = do
    liftIO $ atomically $ modifyTVar' (databaseValuesLock db) (const True)

withShakeDatabaseValuesLock :: ShakeDatabase -> IO c -> IO c
withShakeDatabaseValuesLock sdb act = do
    UE.bracket_ (lockShakeDatabaseValues sdb) (unlockShakeDatabaseValues sdb) act

dbNotLocked :: Database -> STM ()
dbNotLocked db = do
 check =<< readTVar (databaseValuesLock db)



getShakeQueue :: ShakeDatabase -> DBQue
getShakeQueue (ShakeDatabase _ _ db) = databaseQueue db
---------------------------------------------------------------------
-- Keys
newtype Value = Value Dynamic

data KeyDetails = KeyDetails {
    keyStatus      :: !Status,
    keyReverseDeps :: !KeySet
    }

onKeyReverseDeps :: (KeySet -> KeySet) -> KeyDetails -> KeyDetails
onKeyReverseDeps f it@KeyDetails{..} =
    it{keyReverseDeps = f keyReverseDeps}


type DBQue = TaskQueue (Either Dynamic (IO ()))
raedAllLeftsDBQue :: DBQue -> STM [Dynamic]
raedAllLeftsDBQue q = do
    allResult <- flushTaskQueue q
    let (allLeft, allRight) = partitionEithers allResult
    mapM_ (writeTaskQueue q . Right) allRight
    return allLeft




data Database = Database {
    databaseExtra       :: Dynamic,

    databaseThreads     :: TVar [(DeliverStatus, Async ())],

    databaseRuntimeDep  :: SMap.Map Key KeySet,
    databaseRRuntimeDep :: SMap.Map Key KeySet,
    -- it is used to compute the transitive reverse deps, so
    -- if not in any of the transitive reverse deps of a dirty node, it is clean
    -- we can skip clean the threads.
    -- this is update right before we query the database for the key result.
    dataBaseLogger      :: String -> IO (),

    databaseQueue       :: DBQue,

    databaseRules       :: TheRules,
    databaseStep        :: !(TVar Step),

    databaseValuesLock  :: !(TVar Bool),
    -- when we restart a build, we set this to False to block any other
    -- threads from reading databaseValues
    databaseValues      :: !(Map Key KeyDetails)

    }

withWaitingOnKey :: Database -> Key -> Key -> IO b -> IO b
withWaitingOnKey Database{..} pk k ioAct = do
    -- insert the dependency
    -- atomically $ SMap.focus (Focus.alter (Just . maybe (singletonKeySet k) (insertKeySet k))) pk databaseRuntimeDep
    r <- ioAct
    -- remove the one dependency
    -- atomically $ SMap.focus (Focus.alter (fmap (deleteKeySet k))) pk databaseRuntimeDep
    return r


---------------------------------------------------------------------
-- | Remove finished asyncs from 'databaseThreads' (non-blocking).
--   Uses 'poll' to check completion without waiting.
pruneFinished :: Database -> IO ()
pruneFinished db@Database{..} = do
    threads <- readTVarIO databaseThreads
    statuses <- forM threads $ \(d,a) -> do
        p <- poll a
        return (d,a,p)
    let still = [ (d,a) | (d,a,p) <- statuses, isNothing p ]
    -- deleteDatabaseRuntimeDep of finished async keys
    forM_ statuses $ \(d,_,p) -> when (isJust p) $ do
        let k = deliverKey d
        when (k /= newKey "root") $ atomically $ deleteDatabaseRuntimeDep k db
    atomically $ modifyTVar' databaseThreads (const still)

deleteDatabaseRuntimeDep :: Key -> Database -> STM ()
deleteDatabaseRuntimeDep k db = do
    SMap.delete k (databaseRuntimeDep db)


-- compute the transitive reverse dependencies of a set of keys
-- using databaseRuntimeDep in the Database
-- compute the transitive reverse dependencies of a set of keys
-- using databaseRuntimeDep in the Database
computeTransitiveReverseDeps :: Database -> KeySet -> STM KeySet
computeTransitiveReverseDeps db seeds = do
--   rev <- computeReverseRuntimeMap d
  let -- BFS worklist starting from all seed keys.
      -- visited contains everything we've already enqueued (including seeds).
      go :: KeySet -> [Key] -> STM KeySet
      go visited []       = pure visited
      go visited (k:todo) = do
        mDeps <- SMap.lookup k (databaseRRuntimeDep db)
        case mDeps of
          Nothing     -> go visited todo
          Just direct ->
            -- new keys = direct dependents not seen before
            let newKs    = filter (\x -> not (memberKeySet x visited)) (toListKeySet direct)
                visited' = foldr insertKeySet visited newKs
            in go visited' (newKs ++ todo)

  -- Start with seeds already marked visited to prevent self-revisit.
  go seeds (toListKeySet seeds)


insertdatabaseRuntimeDep :: Key -> Key -> Database -> STM ()
insertdatabaseRuntimeDep k pk db = do
    SMap.focus (Focus.alter (Just . maybe (singletonKeySet pk) (insertKeySet pk))) k (databaseRRuntimeDep db)

getDatabaseRuntimeDep :: Database -> Key -> STM KeySet
getDatabaseRuntimeDep db k = do
    mDeps <- SMap.lookup k (databaseRuntimeDep db)
    return $ fromMaybe mempty mDeps
---------------------------------------------------------------------

shakeDataBaseQueue :: ShakeDatabase -> DBQue
shakeDataBaseQueue = databaseQueue . (\(ShakeDatabase _ _ db) -> db)

awaitRunInDb :: Database -> IO result -> IO result
awaitRunInDb db act = awaitRunInThread (databaseQueue db) act


databaseGetActionQueueLength :: Database -> STM Int
databaseGetActionQueueLength db = do
    counTaskQueue (databaseQueue db)

runInDataBase :: String -> Database -> [(IO result, Either SomeException result -> IO ())] -> STM ()
runInDataBase title db acts = do
    s <- getDataBaseStepInt db
    let actWithEmptyHook = map (\(x, y) -> (const $ return (), x, y)) acts
    runInThreadStmInNewThreads db (return $ DeliverStatus s title (newKey "root"))  actWithEmptyHook

runInThreadStmInNewThreads ::  Database -> IO DeliverStatus -> [(Async () -> IO (), IO result, Either SomeException result -> IO ())] -> STM ()
runInThreadStmInNewThreads db mkDeliver acts = do
  -- Take an action from TQueue, run it and
  -- use barrier to wait for the result
    let log prefix title = dataBaseLogger db (prefix ++ title)
    writeTaskQueue (databaseQueue db) $ Right $ do
        uninterruptibleMask $ \restore -> do
            do
                deliver <- mkDeliver
                log "runInThreadStmInNewThreads submit begin " (deliverName deliver)
                curStep <- atomically $ getDataBaseStepInt db
                if curStep == deliverStep deliver then do
                    syncs <- mapM (\(preHook, act, handler) -> do
                        a <- async (handler =<< (restore (Right <$> act) `catch` \e@(SomeException _) -> return (Left e)))
                        preHook a
                        return (deliver, a)
                        ) acts
                    atomically $ modifyTVar' (databaseThreads db) (syncs++)
                else do
                    -- someone might be waiting for something that cancelled, but did not get notified
                    -- because it is not only recorded in the runtime deps

                    -- if it the wait is issue before restart, it would be recorded in the runtime deps
                    -- if it is issued after restart, might not be recorded and causing a problem
                    return ()
                    -- mapM_ (\(_preHook, _act, handler) ->  handler (Left $ SomeException AsyncCancelled)) acts
                log "runInThreadStmInNewThreads submit end " (deliverName deliver)

runInThreadStmInNewThreads1 ::  Database -> IO DeliverStatus -> (Async () -> IO ()) -> IO result -> (Either SomeException result -> IO ()) -> IO ()
runInThreadStmInNewThreads1 db mkDeliver preHook  act  handler = do
        -- Take an action from TQueue, run it and
        -- use barrier to wait for the result
        let log prefix title = dataBaseLogger db (prefix ++ title)
        uninterruptibleMask $ \restore -> do
            do
                deliver <- mkDeliver
                log "runInThreadStmInNewThreads submit begin " (deliverName deliver)
                a <- async (handler =<< (restore (Right <$> act) `catch` \e@(SomeException _) -> return (Left e)))
                preHook a
                atomically $ modifyTVar' (databaseThreads db) ((deliver, a):)
                log "runInThreadStmInNewThreads submit end " (deliverName deliver)

runOneInDataBase :: IO DeliverStatus -> Database -> (Async () -> IO ()) -> IO result -> (Either SomeException result -> IO ()) -> STM ()
runOneInDataBase mkDelivery db registerAsync act handler = do
  runInThreadStmInNewThreads
    db
    mkDelivery
    [ ( registerAsync, act, handler) ]


getDataBaseStepInt :: Database -> STM Int
getDataBaseStepInt db = do
    Step s <- readTVar $ databaseStep db
    return s

data AsyncParentKill = AsyncParentKill ThreadId Step
    deriving (Show, Eq)

instance Exception AsyncParentKill where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

shutDatabase ::Set (Async ()) -> Database -> IO ()
shutDatabase preserve db@Database{..} = uninterruptibleMask $ \unmask -> do
    -- wait for all threads to finish
    asyncs <- readTVarIO databaseThreads
    step <- readTVarIO databaseStep
    tid <- myThreadId
    -- traceEventIO ("shutDatabase: cancelling " ++ show (length asyncs) ++ " asyncs, step " ++ show step)
    -- traceEventIO ("shutDatabase: async entries: " ++ show (map (deliverName . fst) asyncs))
    let remains = filter (\(_, s) -> s `S.member` preserve) asyncs
    let toCancel = filter (\(_, s) -> s `S.notMember` preserve) asyncs
    traceEventIO ("shutDatabase: remains count: " ++ show (length remains) ++ ", names: " ++ show (map (deliverName . fst) remains))
    traceEventIO ("shutDatabase: toCancel count: " ++ show (length toCancel) ++ ", names: " ++ show (map (deliverName . fst) toCancel))
    mapM_ (\(_, a) -> throwTo (asyncThreadId a) $ AsyncParentKill tid step) toCancel
    atomically $ modifyTVar' databaseThreads (const remains)
    -- Wait until all the asyncs are done
    -- But if it takes more than 10 seconds, log to stderr
    unless (null asyncs) $ do
        let warnIfTakingTooLong = unmask $ forever $ do
                sleep 5
                as <- readTVarIO databaseThreads
                -- poll each async: Nothing => still running
                statuses <- forM as $ \(d,a) -> do
                    p <- poll a
                    return (d, a, p)
                let still = [ (deliverName d, show (asyncThreadId a)) | (d,a,p) <- statuses, isNothing p ]
                traceEventIO $ "cleanupAsync: waiting for asyncs to finish; total=" ++ show (length as) ++ ", stillRunning=" ++ show (length still)
                traceEventIO $ "cleanupAsync: still running (deliverName, threadId) = " ++ show still
        withAsync warnIfTakingTooLong $ \_ -> mapM_ (waitCatch . snd) toCancel
    pruneFinished db

-- fdsfsifjsflksfjslthat dmake musch more sense to me
-- peekAsyncsDelivers :: Database -> IO [DeliverStatus]
peekAsyncsDelivers :: MonadIO m => Database -> m [DeliverStatus]
peekAsyncsDelivers db = do
    asyncs <- readTVarIO (databaseThreads db)
    return $ fst <$> asyncs

getDatabaseValues :: Database -> IO [(Key, Status)]
getDatabaseValues = atomically
                  . (fmap.fmap) (second keyStatus)
                  . ListT.toList
                  . SMap.listT
                  . databaseValues

-- todo if stage1 runtime as dirty since it is not yet submitted to the task queue
data RunningStage = RunningStage1 | RunningStage2 (Async ())
    deriving (Eq, Ord)
data Status
    = Clean !Result
    -- todo
    -- dirty should say why it is dirty,
    -- it should and only should be clean,
    -- once all the event has been processed,
    -- once event is represeted by a step
    | Dirty (Maybe Result)
    | Running {
        runningStep  :: !Step,
        -- runningResult :: Result,     -- LAZY
        runningPrev  :: !(Maybe Result),
        runningWait  :: !(MVar (Either SomeException (Key, Result))),
        runningStage :: !RunningStage
        }

viewDirty :: Step -> Status -> Status
-- viewDirty currentStep (Running s re _ _) | currentStep /= s = Dirty re
viewDirty _ other = other


viewToRun :: Step -> Status -> Maybe Status
-- viewToRun _currentStep (Dirty _) = Nothing
-- viewToRun currentStep (Running s _re _ _) | currentStep /= s = Nothing
viewToRun _ other = Just other

getResult :: Status -> Maybe Result
getResult (Clean re)           = Just re
getResult (Dirty m_re)         = m_re
getResult (Running _ m_re _ _) = m_re -- watch out: this returns the previous result


data Result = Result {
    resultValue     :: !Value,
    resultBuilt     :: !Step, -- ^ the step when it was last recomputed
    resultChanged   :: !Step, -- ^ the step when it last changed
    resultVisited   :: !Step, -- ^ the step when it was last looked up
    resultDeps      :: !ResultDeps,
    resultExecution :: !Seconds, -- ^ How long it took, last time it ran
    resultData      :: !BS.ByteString
    }

-- Notice, invariant to maintain:
-- the ![KeySet] in ResultDeps need to be stored in reverse order,
-- so that we can append to it efficiently, and we need the ordering
-- so we can do a linear dependency refreshing in refreshDeps.
data ResultDeps = UnknownDeps | AlwaysRerunDeps !KeySet | ResultDeps ![KeySet]
  deriving (Eq, Show)

getResultDepsDefault :: KeySet -> ResultDeps -> KeySet
getResultDepsDefault _ (ResultDeps ids)      = fold ids
getResultDepsDefault _ (AlwaysRerunDeps ids) = ids
getResultDepsDefault def UnknownDeps         = def

mapResultDeps :: (KeySet -> KeySet) -> ResultDeps -> ResultDeps
mapResultDeps f (ResultDeps ids)      = ResultDeps $ fmap f ids
mapResultDeps f (AlwaysRerunDeps ids) = AlwaysRerunDeps $ f ids
mapResultDeps _ UnknownDeps           = UnknownDeps

instance Semigroup ResultDeps where
    UnknownDeps <> x = x
    x <> UnknownDeps = x
    AlwaysRerunDeps ids <> x = AlwaysRerunDeps (ids <> getResultDepsDefault mempty x)
    x <> AlwaysRerunDeps ids = AlwaysRerunDeps (getResultDepsDefault mempty x <> ids)
    ResultDeps ids <> ResultDeps ids' = ResultDeps (ids <> ids')

instance Monoid ResultDeps where
    mempty = UnknownDeps

---------------------------------------------------------------------
-- Running builds

-- | What mode a rule is running in, passed as an argument to 'BuiltinRun'.
data RunMode
    = RunDependenciesSame -- ^ My dependencies have not changed.
    | RunDependenciesChanged -- ^ At least one of my dependencies from last time have changed, or I have no recorded dependencies.
      deriving (Eq,Show)

instance NFData RunMode where rnf x = x `seq` ()

-- | How the output of a rule has changed.
data RunChanged
    = ChangedNothing -- ^ Nothing has changed.
    | ChangedRecomputeSame -- ^ I recomputed the value and it was the same.
    | ChangedRecomputeDiff -- ^ I recomputed the value and it was different.
      deriving (Eq,Show,Generic)
      deriving anyclass (FromJSON, ToJSON)

instance NFData RunChanged where rnf x = x `seq` ()

-- | The result of 'BuiltinRun'.
data RunResult value = RunResult
    {runChanged :: RunChanged
        -- ^ How has the 'RunResult' changed from what happened last time.
    ,runStore   :: BS.ByteString
        -- ^ The value to store in the Shake database.
    ,runValue   :: value
        -- ^ The value to return from 'Development.Shake.Rule.apply'.
    ,runHook    :: STM ()
        -- ^ The hook to run at the end of the build in the same transaction
        -- when the key is marked as clean.
    } deriving Functor

---------------------------------------------------------------------
-- EXCEPTIONS

data GraphException = forall e. Exception e => GraphException {
    target :: String, -- ^ The key that was being built
    stack  :: [String], -- ^ The stack of keys that led to this exception
    inner  :: e -- ^ The underlying exception
}
  deriving (Exception)

instance Show GraphException where
    show GraphException{..} = unlines $
        ["GraphException: " ++ target] ++
        stack ++
        ["Inner exception: " ++ show inner]

fromGraphException :: Typeable b => SomeException -> Maybe b
fromGraphException x = do
    GraphException _ _ e <- fromException x
    cast e

---------------------------------------------------------------------
-- CALL STACK

data Stack = Stack [Key] !KeySet

instance Show Stack where
    show (Stack kk _) = "Stack: " <> intercalate " -> " (map show kk)

newtype StackException = StackException Stack
  deriving (Show)

instance Exception StackException where
    fromException = fromGraphException
    toException this@(StackException (Stack stack _)) = toException $
        GraphException (show$ last stack) (map show stack) this

addStack :: Key -> Stack -> Either StackException Stack
addStack k (Stack ks is)
    | k `memberKeySet` is = Left $ StackException stack2
    | otherwise = Right stack2
    where stack2 = Stack (k:ks) (insertKeySet k is)

memberStack :: Key -> Stack -> Bool
memberStack k (Stack _ ks) = k `memberKeySet` ks

emptyStack :: Stack
emptyStack = Stack [] mempty
---------------------------------------------------------------------
-- INSTANCES

instance Semigroup a => Semigroup (Rules a) where
    a <> b = liftA2 (<>) a b

instance Monoid a => Monoid (Rules a) where
    mempty = pure mempty
