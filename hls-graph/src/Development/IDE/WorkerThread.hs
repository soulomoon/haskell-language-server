{-
Module : Development.IDE.WorkerThread
Author : @soulomoon
SPDX-License-Identifier: Apache-2.0

Description : This module provides an API for managing worker threads in the IDE.
see Note [Serializing runs in separate thread]
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Development.IDE.WorkerThread
  ( LogWorkerThread (..),
    DeliverStatus(..),
    withWorkerQueue,
    TaskQueue(..),
    withWorkersQueue,
    writeTaskQueue,
    withWorkerQueueSimple,
    isEmptyTaskQueue,
    counTaskQueue,
    submitWork,
    eitherWorker,
    Worker,
    tryReadTaskQueue,
    withWorkerQueueSimpleRight,
    submitWorkAtHead,
    awaitRunInThread,
    withAsyncs,
    haltWokerQueue,
    startWokerQueue,
    stealWorkFromQue
  ) where

import           Control.Concurrent.Async           (Async, async, cancel,
                                                     race_, withAsync)
import           Control.Concurrent.STM
import           Control.Exception.Safe             (SomeException, finally,
                                                     throw, try)
import           Control.Monad.Cont                 (ContT (ContT))
import qualified Data.Text                          as T

import           Control.Concurrent
import           Control.Monad                      (forM, forM_, replicateM_,
                                                     void)
import           Data.Dynamic                       (Dynamic)
import           Development.IDE.Graph.Internal.Key (Key)
import           Prettyprinter


data LogWorkerThread
  = LogThreadEnding !T.Text
  | LogThreadEnded !T.Text
  | LogSingleWorkStarting !T.Text
  | LogSingleWorkEnded !T.Text
  | LogMainThreadId !T.Text !ThreadId
  | LogWorkerText !T.Text
  deriving (Show)

instance Pretty LogWorkerThread where
  pretty = \case
    LogThreadEnding t -> "Worker thread ending:" <+> pretty t
    LogThreadEnded t -> "Worker thread ended:" <+> pretty t
    LogSingleWorkStarting t -> "Worker starting a unit of work: " <+> pretty t
    LogSingleWorkEnded t -> "Worker ended a unit of work: " <+> pretty t
    LogMainThreadId t tid -> "Main thread for" <+> pretty t <+> "is" <+> pretty (show tid)
    LogWorkerText t -> pretty t


{-
Note [Serializing runs in separate thread]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We often want to take long-running actions using some resource that cannot be shared.
In this instance it is useful to have a queue of jobs to run using the resource.
Like the db writes, session loading in session loader, shake session restarts.

Originally we used various ways to implement this, but it was hard to maintain and error prone.
Moreover, we can not stop these threads uniformly when we are shutting down the server.
-}

data TaskQueue a = TaskQueue
  { tqQueue          :: TQueue a
  , tgStartWorker    :: STM Bool -> IO ()
  , tgRunningWorkers :: TVar [Async ()]
  , tgExitMVar       :: TMVar ()
  , tgLog            :: LogWorkerThread -> IO ()
  , tgTitle          :: T.Text
  }

stealWorkFromQue :: TaskQueue a -> STM Bool -> IO ()
stealWorkFromQue TaskQueue{tgStartWorker = extractWorkUntil} = extractWorkUntil

haltWokerQueue :: TaskQueue a -> IO ()
haltWokerQueue TaskQueue{..} = do
    tgLog $ LogWorkerText $ "canceling worker: " <> tgTitle
    workers <- readTVarIO tgRunningWorkers
    void $ atomically $ tryPutTMVar tgExitMVar ()
    forM_ workers cancel
    tgLog $ LogWorkerText $ "cancelled worker: " <> tgTitle

startWokerQueue :: TaskQueue a -> IO ()
startWokerQueue TaskQueue{..} = do
    tgLog $ LogWorkerText $ "Starting worker: " <> tgTitle
    void $ atomically $ tryTakeTMVar tgExitMVar
    newAsync <- async $ tgStartWorker (return False)
    atomically $ modifyTVar tgRunningWorkers (newAsync :)
    tgLog $ LogWorkerText $ "Started worker: " <> tgTitle


-- newTaskQueueIO :: IO (TaskQueue a)
-- newTaskQueueIO :: T.Text -> (t -> IO ()) -> (LogWorkerThread -> IO ()) -> TMVar () -> TVar Bool -> IO (TaskQueue t)
newTaskQueueIO title workerAction log = do
  q <- newTQueueIO
  -- TaskQueue q (worker, start running, halt running)
  -- modify the running TVar to stop/start the worker
  -- not the exitMVar, which is used to stop the worker permanently
  workerSlot <- newTVarIO []
  exitMVar <- newEmptyTMVarIO
  return (TaskQueue {
    tqQueue = q
    , tgStartWorker = \k -> extractWorkUntil exitMVar q k 0
    , tgRunningWorkers = workerSlot
    , tgExitMVar = exitMVar
    , tgLog = log
    , tgTitle = title
    })
  where
    extractWorkUntil exitMVar q k i = do
      task <- atomically $ do
        stealDone <- k
        if stealDone
          then return Exit
          else do
            -- void $ check <$> readTVar running
            task <- tryReadTQueue q
            isEm <- isEmptyTMVar exitMVar
            case (isEm, task) of
              (False, _)   -> return Exit -- stop flag set, exit
              (_, Just t)  -> return $ Task t -- got a task, run it
              (_, Nothing) -> retry -- no task, wait
      case task of
        Exit -> return ()
        Task t -> do
          void $ log $ LogSingleWorkStarting title
          void $ workerAction t
          void $ log $ LogSingleWorkEnded title
          extractWorkUntil exitMVar q k i

data ExitOrTask t = Exit | Task t
type Logger = LogWorkerThread -> IO ()

-- | 'withWorkerQueue' creates a new 'TQueue', and launches a worker
-- thread which polls the queue for requests and runs the given worker
-- function on them.
withWorkerQueueSimple :: Logger -> T.Text -> ContT () IO (TaskQueue (IO ()))
withWorkerQueueSimple log title = withWorkerQueue log title id

withWorkerQueueSimpleRight :: Logger -> T.Text -> ContT () IO (TaskQueue (Either Dynamic (IO ())))
withWorkerQueueSimpleRight log title = withWorkerQueue log title $ eitherWorker (const $ return ()) id


withWorkerQueue :: Logger -> T.Text -> (t -> IO ()) -> ContT () IO (TaskQueue t)
withWorkerQueue = withWorkersQueue 1
withWorkersQueue :: Int -> Logger -> T.Text -> (t -> IO ()) -> ContT () IO (TaskQueue t)
withWorkersQueue n log title workerAction = ContT $ \mainAction -> do
  tid <- myThreadId
  log (LogMainThreadId title tid)
  -- Use a TMVar as a stop flag to coordinate graceful shutdown.
  -- The worker thread checks this flag before dequeuing each job; if set, it exits immediately,
  -- ensuring that no new work is started after shutdown is requested.
  -- This mechanism is necessary because some downstream code may swallow async exceptions,
  -- making 'cancel' unreliable for stopping the thread in all cases.
  -- If 'cancel' does interrupt the thread (e.g., while blocked in STM or in a cooperative job),
  -- the thread exits immediately and never checks the TMVar; in such cases, the stop flag is redundant.
  q <- newTaskQueueIO title workerAction log
  -- start the number of workers
  replicateM_ n $ startWokerQueue q
  mainAction q
    -- if we want to debug the exact location the worker swallows an async exception, we can
    -- temporarily comment out the `finally` clause.
        `finally` do
            log (LogThreadEnding title)
            haltWokerQueue q
            log (LogThreadEnded title)

withAsyncs :: [IO ()] -> IO () -> IO ()
withAsyncs ios mainAction = go ios
    where
        go []     = mainAction
        go (x:xs) = withAsync x $ \_ -> go xs

-- | 'awaitRunInThread' queues up an 'IO' action to be run by a worker thread,
-- and then blocks until the result is computed. If the action throws an
-- non-async exception, it is rethrown in the calling thread.

data DeliverStatus = DeliverStatus
  { deliverStep   :: Int
    , deliverName :: String
    , deliverKey  :: Key
  } deriving (Show)

instance Pretty DeliverStatus where
  pretty (DeliverStatus step _name key) =
    "Step:" <+> pretty step <> "," <+> "Key:" <+> pretty (show key)


type Worker arg = arg -> IO ()

eitherWorker :: Worker a -> Worker b -> Worker (Either a b)
eitherWorker w1 w2 = \case
  Left a  -> w1 a
  Right b -> w2 b

awaitRunInThread :: TaskQueue (Either Dynamic (IO ())) -> IO result -> IO result
awaitRunInThread TaskQueue{tqQueue = q} act = do
  barrier <- newEmptyTMVarIO
  -- Take an action from TQueue, run it and
  -- use barrier to wait for the result
  atomically $ writeTQueue q (Right $ try act >>= atomically . putTMVar barrier)
  resultOrException <- atomically $ takeTMVar barrier
  case resultOrException of
    Left e  -> throw (e :: SomeException)
    Right r -> return r


-- submitWork without waiting for the result
submitWork :: TaskQueue arg -> arg -> IO ()
submitWork TaskQueue{tqQueue = q} arg = atomically $ writeTQueue q arg

-- submit work at the head of the queue, so it will be executed next
submitWorkAtHead :: TaskQueue arg -> arg -> IO ()
submitWorkAtHead TaskQueue{tqQueue = q} arg = do
  atomically $ unGetTQueue q arg

writeTaskQueue :: TaskQueue a -> a -> STM ()
writeTaskQueue TaskQueue{tqQueue = q} = writeTQueue q

tryReadTaskQueue :: TaskQueue a -> STM (Maybe a)
tryReadTaskQueue TaskQueue{tqQueue = q} = tryReadTQueue q

isEmptyTaskQueue :: TaskQueue a -> STM Bool
isEmptyTaskQueue TaskQueue{tqQueue = q} = isEmptyTQueue q

-- look and count the number of items in the queue
-- do not remove them
counTaskQueue :: TaskQueue a -> STM Int
counTaskQueue TaskQueue{tqQueue = q} = do
  xs <- flushTQueue q
  mapM_ (unGetTQueue q) (reverse xs)
  return $ length xs

