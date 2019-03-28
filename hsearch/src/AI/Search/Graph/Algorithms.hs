module AI.Search.Graph.Algorithms 
    ( breadthFirstSearch
    , uniformCostSearch
    , depthFirstSearch
    , greedyBestFirstSearch
    , aStarSearch
    ) where

import           AI.Search.Graph.Generics
import qualified AI.Search.Policies as P
import qualified Data.AI.Search.SearchNode as SN

-- TREE SEARCH --

-- NOT INFORMED SEARCH --

-- | search with breadth first policy. The root node is expanded first, then all its childrens and so
--   on, It's /complete/ and /optimal/ but very expensive in time and space complexity (/O(b^d)/ with
--   b branching factor and d minimum depth goal)
{-# INLINE breadthFirstSearch #-}
breadthFirstSearch :: (Ord s, Num p) => (s -> Bool)       -- ^ function used to check if the goal is reached
                                    -> (s -> [(s, p)])   -- ^ generator of states
                                    -> s                 -- ^ initial state
                                    -> [s]               -- ^ returns list of states
breadthFirstSearch = search SN.state Forever P.breadthFirstPolicy Generation

-- | generalization of @breadthFirstSearch@ when paths costs are not all equals on the same level
{-# INLINE uniformCostSearch #-}
uniformCostSearch :: (Ord s, Num p, Ord p) => (s -> Bool) -- ^ function used to check if the goal is reached
                                   -> (s -> [(s, p)])    -- ^ generator of states
                                   -> s                  -- ^ initial state
                                   -> [s]                -- ^ returns list of states
uniformCostSearch = search SN.state Forever P.uniformCostPolicy Expansion

-- | search with depth first policy. The deepest node is always expanded first, it is not /complete/ or
--   /optimal/ but keeps only one route at a time in memory so it has a spatial complexity of /O(bm)/ with
--   b branching factor and m max depth in the state space (can be infinite). If in the state graph there are
--   loops then it will loop forever. Time complexity is /O(b^m)/.
{-# INLINE depthFirstSearch #-}
depthFirstSearch :: (Ord s, Num p) => (s -> Bool) -- ^ function used to check if the goal is reached
                           -> (s -> [(s, p)])    -- ^ generator of states
                           -> s                  -- ^ initial state
                           -> [s]                -- ^ returns list of states
depthFirstSearch = search SN.state Forever P.depthFirstPolicy Generation

-- INFORMED SEARCH ALGORITHMS --

-- | informed search algorithm which take into account only the value of the heuristic function in order to 
--   choose next state, it is greedy cause it tries to reach the goal as fast as possible.It is not /optimal/
--   and can not be /complete/, but really depends on the heuristic function.
{-# INLINE greedyBestFirstSearch #-}
greedyBestFirstSearch :: (Ord s, Num p, Ord p) => (s -> p)        -- ^ heuristic cost function
                                              -> (s -> Bool)       -- ^ function used to check if the goal is reached
                                              -> (s -> [(s, p)]) -- ^ generator of states
                                              -> s                 -- ^ initial state
                                              -> [s]               -- ^ returns list of states
greedyBestFirstSearch h = search SN.state Forever (P.greedyBestFirstPolicy h) Expansion

-- | A* search is a informed search algorithm which take into account both cost to reach a node and heuristic function
--   in order to choose next state. It behaves as a @uniformCostSearch@ with a heuristic function
{-# INLINE aStarSearch #-}
aStarSearch :: (Ord s, Num p, Ord p) => (s -> p)          -- ^ heuristic cost function
                                    -> (s -> Bool)       -- ^ function used to check if the goal is reached
                                    -> (s -> [(s, p)])   -- ^ generator of states
                                    -> s                 -- ^ initial state
                                    -> [s]               -- ^ returns list of states
aStarSearch h = search SN.state Forever (P.aStarPolicy h) Expansion
