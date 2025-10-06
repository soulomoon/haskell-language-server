{-# LANGUAGE RecordWildCards #-}

-- | Incremental topological ordering using the Pearce-Kelly algorithm
--
-- The Pearce-Kelly algorithm maintains a topological order of a DAG incrementally.
-- Each node is assigned an integer order value. When edges are added, the algorithm
-- efficiently reorders only the affected nodes to maintain topological consistency.
--
-- Reference: "An Incremental Algorithm for Maintaining the Topological Order of
-- a Directed Acyclic Graph" by Pearce and Kelly (2006)
module Development.IDE.Graph.Internal.TopoSort
    ( emptyTopoOrder
    , addEdge
    , removeKey
    , lookupOrder
    , getAffectedKeysInOrder
    ) where

import           Control.Concurrent.STM.Stats (STM, atomically, readTVar,
                                               writeTVar)
import           Control.Monad                (when)
import           Data.List                    (sortOn)
import           Data.Maybe                   (mapMaybe)
import           Development.IDE.Graph.Internal.Types (TopoOrder (..))
import           Development.IDE.Graph.KeySet
import qualified Focus
import qualified StmContainers.Map            as SMap
import           UnliftIO                     (newTVarIO)

-- | Create an empty topological order
emptyTopoOrder :: IO TopoOrder
emptyTopoOrder = do
    topoOrderMap <- atomically SMap.new
    topoNextOrderNum <- newTVarIO 0
    return TopoOrder{..}

-- | Look up the order of a key
lookupOrder :: TopoOrder -> Key -> STM (Maybe Int)
lookupOrder TopoOrder{..} key = SMap.lookup key topoOrderMap

-- | Get affected keys from a KeySet, in topological order
getAffectedKeysInOrder :: TopoOrder -> KeySet -> STM [Key]
getAffectedKeysInOrder TopoOrder{..} affected = do
    let affectedList = toListKeySet affected
    withOrders <- mapM (\k -> do
        mord <- SMap.lookup k topoOrderMap
        return $ (\o -> (k, o)) <$> mord) affectedList
    return $ map fst $ sortOn snd $ mapMaybe id withOrders

-- | Ensure a key has an order assigned
ensureOrder :: TopoOrder -> Key -> STM Int
ensureOrder TopoOrder{..} key = do
    mord <- SMap.lookup key topoOrderMap
    case mord of
        Just ord -> return ord
        Nothing -> do
            nextOrd <- readTVar topoNextOrderNum
            writeTVar topoNextOrderNum (nextOrd + 1)
            SMap.insert nextOrd key topoOrderMap
            return nextOrd

-- | Add an edge and maintain topological order using Pearce-Kelly
-- In the dependency graph: edge from 'from' to 'to' means 'from' depends on 'to'
-- In topological order: 'to' must come before 'from' (to has smaller order)
addEdge :: TopoOrder -> (Key -> STM (Maybe KeySet)) -> Key -> Key -> STM ()
addEdge topo@TopoOrder{..} getRDeps from to = do
    fromOrd <- ensureOrder topo from
    toOrd <- ensureOrder topo to
    -- If 'to' already comes before 'from', order is correct
    -- Otherwise, need to reorder using Pearce-Kelly forward search
    when (fromOrd <= toOrd) $ do
        -- Forward search: find all keys that transitively depend on 'from'
        -- These need to be shifted to maintain topological order
        affected <- forwardReach topo getRDeps from
        affectedWithOrders <- mapM (\k -> do
            mord <- SMap.lookup k topoOrderMap
            return $ (\o -> (k, o)) <$> mord) affected
        let affectedPairs = mapMaybe id affectedWithOrders
        -- Only reorder if we have affected keys
        when (not $ null affectedPairs) $ do
            let minAffected = minimum $ map snd affectedPairs
            -- Shift affected keys to come after 'to'
            when (minAffected <= toOrd) $ do
                let shift = toOrd - minAffected + 1
                mapM_ (\k -> SMap.focus (Focus.adjust (+ shift)) k topoOrderMap) affected

-- | Forward reachability: find all keys that transitively depend on a given key
-- Uses DFS through reverse dependencies
forwardReach :: TopoOrder -> (Key -> STM (Maybe KeySet)) -> Key -> STM [Key]
forwardReach _topo getRDeps start = go [start] mempty []
  where
    go [] _visited acc = return acc
    go (k:ks) visited acc
        | k `memberKeySet` visited = go ks visited acc
        | otherwise = do
            let visited' = insertKeySet k visited
            mrdeps <- getRDeps k
            let rdeps = maybe [] toListKeySet mrdeps
            go (rdeps ++ ks) visited' (k : acc)

-- | Remove a key from the topological order
removeKey :: TopoOrder -> Key -> STM ()
removeKey TopoOrder{..} key = SMap.delete key topoOrderMap
