-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Development.IDE.Core.Debouncer
    ( Debouncer
    , registerEvent
    , debouncerIsEmpty
    , newAsyncDebouncer
    , noopDebouncer
    ) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM.Stats (STM, TVar, atomically,
                                               atomicallyNamed, modifyTVar',
                                               newTVarIO, readTVar)
import           Control.Exception
import           Control.Monad                (join)
import           Data.Foldable                (traverse_)
import           Data.Hashable
import qualified Focus
import qualified ListT
import qualified StmContainers.Map            as STM
import           System.Time.Extra

-- | A debouncer can be used to avoid triggering many events
-- (e.g. diagnostics) for the same key (e.g. the same file)
-- within a short timeframe. This is accomplished
-- by delaying each event for a given time. If another event
-- is registered for the same key within that timeframe,
-- only the new event will fire.
--
-- We abstract over the debouncer used so we an use a proper debouncer in the IDE but disable
-- debouncing in the DAML CLI compiler.
data Debouncer k = Debouncer
    { registerEvent         :: Seconds -> k -> IO () -> IO ()
    , debouncerPendingCount :: IO Int
    -- ^ Count the currently debounced events.
    --
    -- This includes events waiting for the debounce delay and events whose fire
    -- action is currently running.
    , debouncerIsEmpty      :: STM Bool
    -- ^ Check whether the debouncer has no waiting or running events.
    }

-- | Debouncer used in the IDE that delays events as expected.
newAsyncDebouncer :: (Eq k, Hashable k) => IO (Debouncer k)
newAsyncDebouncer = do
    pending <- STM.newIO
    running <- newTVarIO 0
    return Debouncer
        { registerEvent =  \s k e -> asyncRegisterEvent pending s k
                (bracket_
                  (atomically $ modifyTVar' running (+ 1))
                  (atomically $ modifyTVar' running (subtract 1)) e)
        , debouncerPendingCount = atomically $ countDebouncerEvents pending running
        , debouncerIsEmpty = fmap (== 0) $ countDebouncerEvents pending running
        }

countDebouncerEvents :: STM.Map k (Async ()) -> TVar Int -> STM Int
countDebouncerEvents pending running = do
    pendingCount <- length <$> ListT.toList (STM.listT pending)
    runningCount <- readTVar running
    return $ pendingCount + runningCount

-- | Register an event that will fire after the given delay if no other event
-- for the same key gets registered until then.
--
-- If there is a pending event for the same key, the pending event will be killed.
-- Events are run unmasked so it is up to the user of `registerEvent`
-- to mask if required.
asyncRegisterEvent :: (Eq k, Hashable k) => STM.Map k (Async ()) -> Seconds -> k -> IO () -> IO ()
asyncRegisterEvent d 0 k fire = do
    join $ atomically $ do
        prev <- STM.focus Focus.lookupAndDelete k d
        return $ traverse_ cancel prev
    fire
asyncRegisterEvent d delay k fire = mask_ $ do
    a <- asyncWithUnmask $ \unmask -> unmask $ do
        sleep delay
        fire
        atomically $ STM.delete k d
    prev <- atomicallyNamed "debouncer" $ STM.focus (Focus.lookup <* Focus.insert a) k d
    traverse_ cancel prev

-- | Debouncer used in the DAML CLI compiler that emits events immediately.
noopDebouncer :: Debouncer k
noopDebouncer = Debouncer
    { registerEvent = \_ _ a -> a
    , debouncerPendingCount = return 0
    , debouncerIsEmpty = return True
    }
