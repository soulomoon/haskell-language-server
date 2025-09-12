{-# OPTIONS_GHC -Wno-orphans #-}
module Development.IDE.Types.Action
  ( DelayedAction (..),
    DelayedActionInternal,
    ActionQueue,
    newQueue,
    pushQueue,
    popQueue,
    doneQueue,
    peekInProgress,
    abortQueue,
    countQueue,
    isActionQueueEmpty,
    delayedActionKey)
where

import           Control.Concurrent.STM
import           Data.Hashable          (Hashable (..))
import           Data.HashSet           (HashSet)
import qualified Data.HashSet           as Set
import           Data.Unique            (Unique, hashUnique)
import           Development.IDE.Graph  (Action, Key, newKey)
import           Ide.Logger
import           Numeric.Natural

data DelayedAction a = DelayedAction
  { uniqueID       :: Unique,
    -- | Name we use for debugging
    actionName     :: String,
    -- | Priority with which to log the action
    actionPriority :: Priority,
    -- | The payload
    getAction      :: Action a
  }
  deriving (Functor)

instance Show Unique where
  show = show . hashUnique

type DelayedActionInternal = DelayedAction ()

instance Eq (DelayedAction a) where
  a == b = uniqueID a == uniqueID b

instance Hashable (DelayedAction a) where
  hashWithSalt s = hashWithSalt s . uniqueID

instance Show (DelayedAction a) where
  show d = "DelayedAction: " ++ actionName d

------------------------------------------------------------------------------
delayedActionKey :: DelayedAction a -> Key
delayedActionKey = newKey . show . uniqueID

data ActionQueue = ActionQueue
  { newActions :: TQueue DelayedActionInternal,
    inProgress :: TVar (HashSet DelayedActionInternal)
  }

newQueue :: IO ActionQueue
newQueue = atomically $ do
  newActions <- newTQueue
  inProgress <- newTVar mempty
  return ActionQueue {..}

pushQueue :: DelayedActionInternal -> ActionQueue -> STM ()
pushQueue act ActionQueue {..} = writeTQueue newActions act

-- | You must call 'doneQueue' to signal completion
popQueue :: ActionQueue -> STM DelayedActionInternal
popQueue ActionQueue {..} = do
  x <- readTQueue newActions
  modifyTVar inProgress (Set.insert x)
  return x

-- | Completely remove an action from the queue
abortQueue :: DelayedActionInternal -> ActionQueue -> STM ()
abortQueue x ActionQueue {..} = do
  qq <- flushTQueue newActions
  mapM_ (writeTQueue newActions) (filter (/= x) qq)
  modifyTVar' inProgress (Set.delete x)

-- | Mark an action as complete when called after 'popQueue'.
--   Has no effect otherwise
doneQueue :: DelayedActionInternal -> ActionQueue -> STM ()
doneQueue x ActionQueue {..} = do
  modifyTVar' inProgress (Set.delete x)

countQueue :: ActionQueue -> STM Natural
countQueue ActionQueue{..} = do
    backlog <- flushTQueue newActions
    mapM_ (writeTQueue newActions) backlog
    m <- Set.size <$> readTVar inProgress
    return $ fromIntegral $ length backlog + m

peekInProgress :: ActionQueue -> STM [DelayedActionInternal]
peekInProgress ActionQueue {..} = Set.toList <$> readTVar inProgress

isActionQueueEmpty :: ActionQueue -> STM Bool
isActionQueueEmpty ActionQueue {..} = do
    emptyQueue <- isEmptyTQueue newActions
    inProg <- Set.null <$> readTVar inProgress
    return (emptyQueue && inProg)
