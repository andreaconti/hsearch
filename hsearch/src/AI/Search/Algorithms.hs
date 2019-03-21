module AI.Search.Algorithms 
    ( breadthFirstSearch
    , uniformCostSearch
    , depthFirstSearch
    , iterativeDepthFirstSearch
    , aStarSearch
    , iterativeAStarSearch
    , breadthFirstSearch'
    , uniformCostSearch'
    , depthFirstSearch'
    , iterativeDepthFirstSearch'
    , aStarSearch'
    , iterativeAStarSearch'
    ) where

import           AI.Search.Generics
import qualified AI.Search.Policies as P

-- TREE SEARCH --

-- NOT INFORMED SEARCH --

-- | search with breadth first policy. The root node is expanded first, then all its childrens and so
--   on, It's /complete/ and /optimal/ but very expensive in time and space complexity (/O(b^d)/ with
--   b branching factor and d minimum depth goal)
{-# INLINE breadthFirstSearch #-}
breadthFirstSearch :: (Eq s) => (s -> Bool)       -- ^ function used to check if the goal is reached
                             -> (s -> [(s, Int)]) -- ^ generator of states
                             -> s                 -- ^ initial state
                             -> [s]               -- ^ returns list of states
breadthFirstSearch = search P.breadthFirstPolicy Generation

-- | generalization of @breadthFirstSearch@ when paths costs are not all equals on the same level
{-# INLINE uniformCostSearch #-}
uniformCostSearch :: (Eq s) => (s -> Bool)       -- ^ function used to check if the goal is reached
                            -> (s -> [(s, Int)]) -- ^ generator of states
                            -> s                 -- ^ initial state
                            -> [s]               -- ^ returns list of states
uniformCostSearch = search P.uniformCostPolicy Expansion

-- | search with depth first policy. The deepest node is always expanded first, it is not /complete/ or
--   /optimal/ but keeps only one route at a time in memory so it has a spatial complexity of /O(bm)/ with
--   b branching factor and m max depth in the state space (can be infinite). If in the state graph there are
--   loops then it will loop forever. Time complexity is /O(b^m)/.
{-# INLINE depthFirstSearch #-}
depthFirstSearch :: (Eq s) => (s -> Bool)       -- ^ function used to check if the goal is reached
                           -> (s -> [(s, Int)]) -- ^ generator of states
                           -> s                 -- ^ initial state
                           -> [s]               -- ^ returns list of states
depthFirstSearch = search P.depthFirstPolicy Generation

-- | search with @depthFirstSearch@ in iterative way, If in the state graph there are
--   loops it will not loop forever.
{-# INLINE iterativeDepthFirstSearch #-}
iterativeDepthFirstSearch:: (Eq s) => (s -> Bool)       -- ^ function used to check if the goal is reached
                                   -> (s -> [(s, Int)]) -- ^ generator of states
                                   -> s                 -- ^ initial state
                                   -> [s]               -- ^ returns list of states
iterativeDepthFirstSearch = iterativeSearch P.depthFirstPolicy Generation

-- INFORMED SEARCH ALGORITHMS --

-- | informed search algorithm which take into account only the value of the heuristic function in order to 
--   choose next state, it is greedy cause it tries to reach the goal as fast as possible.It is not /optimal/
--   and can not be /complete/, but really depends on the heuristic function.
{-# INLINE greedyBestFirstSearch #-}
greedyBestFirstSearch :: (Eq s) => (s -> Int)        -- ^ heuristic cost function
                                -> (s -> Bool)       -- ^ function used to check if the goal is reached
                                -> (s -> [(s, Int)]) -- ^ generator of states
                                -> s                 -- ^ initial state
                                -> [s]               -- ^ returns list of states
greedyBestFirstSearch h = search (P.greedyBestFirstPolicy h) Expansion

-- | A* search is a informed search algorithm which take into account both cost to reach a node and heuristic function
--   in order to choose next state. It behaves as a @uniformCostSearch@ with a heuristic function
{-# INLINE aStarSearch #-}
aStarSearch :: (Eq s) => (s -> Int)        -- ^ heuristic cost function
                      -> (s -> Bool)       -- ^ function used to check if the goal is reached
                      -> (s -> [(s, Int)]) -- ^ generator of states
                      -> s                 -- ^ initial state
                      -> [s]               -- ^ returns list of states
aStarSearch h = search (P.aStarPolicy h) Expansion

-- | search with @aStarSearch@ in iterative way
{-# INLINE iterativeAStarSearch #-}
iterativeAStarSearch :: (Eq s) => (s -> Int)        -- ^ heuristic cost function
                               -> (s -> Bool)       -- ^ function used to check if the goal is reached
                               -> (s -> [(s, Int)]) -- ^ generator of states
                               -> s                 -- ^ initial state
                               -> [s]               -- ^ returns list of states
iterativeAStarSearch h = iterativeSearch (P.aStarPolicy h) Expansion

-- GRAPH SEARCH --

-- | search with breadth first policy. The root node is expanded first, then all its childrens and so
--   on, It's /complete/ and /optimal/ but very expensive in time and space complexity (/O(b^d)/ with
--   b branching factor and d minimum depth goal), it discards all yet expanded nodes
{-# INLINE breadthFirstSearch' #-}
breadthFirstSearch' :: (Eq s) => (s -> Bool)       -- ^ function used to check if the goal is reached
                             -> (s -> [(s, Int)]) -- ^ generator of states
                             -> s                 -- ^ initial state
                             -> [s]               -- ^ returns list of states
breadthFirstSearch' = search' P.breadthFirstPolicy Generation

-- | generalization of @breadthFirstSearch@ when paths costs are not all equals on the same level, it
--   discards all yet expanded nodes
{-# INLINE uniformCostSearch' #-}
uniformCostSearch' :: (Eq s) => (s -> Bool)       -- ^ function used to check if the goal is reached
                            -> (s -> [(s, Int)]) -- ^ generator of states
                            -> s                 -- ^ initial state
                            -> [s]               -- ^ returns list of states
uniformCostSearch' = search P.uniformCostPolicy Expansion

-- | search with depth first policy. The deepest node is always expanded first, it is not /complete/ or
--   /optimal/ but keeps only one route at a time in memory so it has a spatial complexity of /O(bm)/ with
--   b branching factor and m max depth in the state space (can be infinite). If in the state graph there are
--   loops then it will loop forever. Time complexity is /O(b^m)/. It discards all yet expanded nodes
{-# INLINE depthFirstSearch' #-}
depthFirstSearch' :: (Eq s) => (s -> Bool)       -- ^ function used to check if the goal is reached
                           -> (s -> [(s, Int)]) -- ^ generator of states
                           -> s                 -- ^ initial state
                           -> [s]               -- ^ returns list of states
depthFirstSearch' = search' P.depthFirstPolicy Generation

-- | search with @depthFirstSearch@ in iterative way, If in the state graph there are
--   loops it will not loop forever. It discards alla yet expanded nodes
{-# INLINE iterativeDepthFirstSearch' #-}
iterativeDepthFirstSearch' :: (Eq s) => (s -> Bool)       -- ^ function used to check if the goal is reached
                                   -> (s -> [(s, Int)]) -- ^ generator of states
                                   -> s                 -- ^ initial state
                                   -> [s]               -- ^ returns list of states
iterativeDepthFirstSearch' = iterativeSearch' P.depthFirstPolicy Generation

-- INFORMED SEARCH ALGORITHMS --

-- | informed search algorithm which take into account only the value of the heuristic function in order to 
--   choose next state, it is greedy cause it tries to reach the goal as fast as possible.It is not /optimal/
--   and can not be /complete/, but really depends on the heuristic function. It discards all yet expanded nodes.
{-# INLINE greedyBestFirstSearch' #-}
greedyBestFirstSearch' :: (Eq s) => (s -> Int)        -- ^ heuristic cost function
                                -> (s -> Bool)       -- ^ function used to check if the goal is reached
                                -> (s -> [(s, Int)]) -- ^ generator of states
                                -> s                 -- ^ initial state
                                -> [s]               -- ^ returns list of states
greedyBestFirstSearch' h = search' (P.greedyBestFirstPolicy h) Expansion

-- | A* search is a informed search algorithm which take into account both cost to reach a node and heuristic function
--   in order to choose next state. It behaves as a @uniformCostSearch@ with a heuristic function. It discards all yet
--   expanded nodes.
{-# INLINE aStarSearch' #-}
aStarSearch' :: (Eq s) => (s -> Int)        -- ^ heuristic cost function
                      -> (s -> Bool)       -- ^ function used to check if the goal is reached
                      -> (s -> [(s, Int)]) -- ^ generator of states
                      -> s                 -- ^ initial state
                      -> [s]               -- ^ returns list of states
aStarSearch' h = search' (P.aStarPolicy h) Expansion

-- | search with @aStarSearch@ in iterative way, it discards alla yet expanded nodes.
{-# INLINE iterativeAStarSearch' #-}
iterativeAStarSearch' :: (Eq s) => (s -> Int)        -- ^ heuristic cost function
                               -> (s -> Bool)       -- ^ function used to check if the goal is reached
                               -> (s -> [(s, Int)]) -- ^ generator of states
                               -> s                 -- ^ initial state
                               -> [s]               -- ^ returns list of states
iterativeAStarSearch' h = iterativeSearch' (P.aStarPolicy h) Expansion
