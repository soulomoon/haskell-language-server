module Development.IDE.Types.Action ( Action
                                                       , Priority(..)
                                                       , DelayedAction(..)
                                                       , DelayedActionInternal
                                                       , ActionQueue
                                                       , newQueue
                                                       , pushQueue
                                                       , popQueue
                                                       , doneQueue
                                                       , peekInProgress
                                                       , abortQueue
                                                       , countQueue
                                                       , isActionQueueEmpty
                                                       , unGetQueue
                                                       , countInProgress) where

import           Control.Concurrent.STM
import           Development.IDE.Graph.Internal.Types (Action, ActionQueue,
                                                       DelayedAction (..),
                                                       Priority (..),
                                                       abortQueue, countQueue,
                                                       doneQueue,
                                                       isActionQueueEmpty,
                                                       newQueue, peekInProgress,
                                                       popQueue, pushQueue,
                                                       unGetQueue)

-- | Alias specialized to the graph Action monad
type DelayedActionInternal = DelayedAction ()
countInProgress :: ActionQueue -> STM Int
countInProgress queue = fmap length $ peekInProgress queue
