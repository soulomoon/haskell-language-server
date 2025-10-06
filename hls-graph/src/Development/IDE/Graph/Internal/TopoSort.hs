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
    , getOrderedList
    , getAffectedKeysInOrder
    ) where

import           Control.Concurrent.STM.Stats (STM, modifyTVar', readTVar,
                                               writeTVar)
import           Control.Monad                (when)
import qualified Data.HashMap.Strict          as Map
import           Data.List                    (sortOn)
import           Data.Maybe                   (mapMaybe)
import           Development.IDE.Graph.Internal.Types (TopoOrder (..))
import           Development.IDE.Graph.KeySet
import           UnliftIO                     (newTVarIO)

-- | Create an empty topological order
emptyTopoOrder :: IO TopoOrder
emptyTopoOrder = do
    topoOrderMap <- newTVarIO Map.empty
    topoNextOrderNum <- newTVarIO 0
    return TopoOrder{..}

-- | Look up the order of a key
lookupOrder :: TopoOrder -> Key -> STM (Maybe Int)
lookupOrder TopoOrder{..} key = do
    orderMap <- readTVar topoOrderMap
    return $ Map.lookup key orderMap

-- | Get all keys sorted by their topological order
getOrderedList :: TopoOrder -> STM [Key]
getOrderedList TopoOrder{..} = do
    orderMap <- readTVar topoOrderMap
    return $ map fst $ sortOn snd $ Map.toList orderMap

-- | Get affected keys from a KeySet, in topological order
getAffectedKeysInOrder :: TopoOrder -> KeySet -> STM [Key]
getAffectedKeysInOrder TopoOrder{..} affected = do
    orderMap <- readTVar topoOrderMap
    let affectedList = toListKeySet affected
        withOrders = mapMaybe (\k -> (\o -> (k, o)) <$> Map.lookup k orderMap) affectedList
    return $ map fst $ sortOn snd withOrders

-- | Ensure a key has an order assigned
ensureOrder :: TopoOrder -> Key -> STM Int
ensureOrder TopoOrder{..} key = do
    orderMap <- readTVar topoOrderMap
    case Map.lookup key orderMap of
        Just ord -> return ord
        Nothing -> do
            nextOrd <- readTVar topoNextOrderNum
            writeTVar topoNextOrderNum (nextOrd + 1)
            modifyTVar' topoOrderMap (Map.insert key nextOrd)
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
        orderMap <- readTVar topoOrderMap
        let affectedWithOrders = mapMaybe (\k -> (\o -> (k, o)) <$> Map.lookup k orderMap) affected
        -- Only reorder if we have affected keys
        when (not $ null affectedWithOrders) $ do
            let minAffected = minimum $ map snd affectedWithOrders
            -- Shift affected keys to come after 'to'
            when (minAffected <= toOrd) $ do
                let shift = toOrd - minAffected + 1
                orderMap' <- readTVar topoOrderMap
                let newMap = foldr (\k m -> Map.adjust (+ shift) k m) orderMap' affected
                writeTVar topoOrderMap newMap

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
removeKey TopoOrder{..} key = do
    modifyTVar' topoOrderMap (Map.delete key)
