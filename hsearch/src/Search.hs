{-# LANGUAGE RankNTypes, BangPatterns #-}

{-|
Module      : Search
Description : Front-end module for search algorithms
Copyright   : 
License     : GPL-3
Maintainer  : andreaconti
Stability   : experimental
Portability : Windows, POSIX
-}

module Search 
    ( MaxDepth(..)
    , searchUntilDepth
    , iterativeSearch
    , search
    , breadthFirstSearch
    , uniformCostSearch
    , depthFirstSearch
    ) where

import qualified Data.Search.SearchNode as SN
import           Data.Search.SearchNode (SNode(..))
import qualified Data.Search.Impl.RootSearchNode as RN
import           Data.Search.Impl.RootSearchNode (RSNode(..))
import           Data.Search.Frontier (Frontier, next, insert)
import qualified Data.Search.Frontier.PQFrontier as PQ

import qualified Search.Policies as P
import           Search.Policies (CheckTime(..))

import           Control.Monad 
import           Control.Applicative
import           Data.Maybe (maybeToList, listToMaybe)

-- TYPES --

-- | max depth to search for
data MaxDepth = Forever | Until Int deriving (Eq, Show)

-- | Goal utility in order to find first goal in a list o nodes
{-# INLINE findGoal #-}
findGoal :: (s -> Bool) -> [s] -> Maybe s
findGoal f = listToMaybe . take 1 . filter f

-- GENERIC SEARCH --

-- | /searchUntilDepth/ provides a high level interface for a generic Search Tree algorithm with custom max
--   depth to search for
searchUntilDepth :: (Eq s, Ord i) => MaxDepth          -- ^ max depth to search
                                  -> (SNode s -> i)    -- ^ policy to be used
                                  -> CheckTime         -- ^ when apply check goal
                                  -> (s -> Bool)       -- ^ function used to check if the goal is reached
                                  -> (s -> [(s, Int)]) -- ^ generator of states
                                  -> s                 -- ^ initial state
                                  -> [s]               -- ^ returns list of states
searchUntilDepth !md p !ct cg g s = join . maybeToList $ loop initFrontier
    where initFrontier = insert (PQ.priorityQueueFrontier (\(RSNode _ x) -> p x)) [RSNode Root (SN.SNode s 0 0)]
          loop fr = do
            (cFr, rsNode@(RSNode _ currentNode@(SNode cs cd cc))) <- next fr
            let 
                checkDepth Forever _   = True
                checkDepth (Until d) x = x < d && d > 0

                newStates    = g cs
                newSNodes    = guard (checkDepth md cd) >> map (\(ns, nc) -> SNode ns (cd+1) (nc+cc)) newStates
                genGoal      = guard (ct == Generation && cg cs) >> (Just $! cs)
                expGoal      = guard (ct == Expansion) >> findGoal cg (map fst newStates)
            case genGoal <|> expGoal of
                Nothing   -> loop (insert cFr $ map (RSNode rsNode) newSNodes)
                Just goal -> return $! map SN.state . RN.rsNodeToList $ rsNode

-- | /iterativeSearch/ is equivalent to @searchUntilDepth@ called on increasing depths until the goal is found. Used with
--   a depth first like policy can provide features similar to a search in amplitude (breadth first like policy)
iterativeSearch :: (Eq s, Ord i) => (SNode s -> i)    -- ^ policy to be used
                                 -> CheckTime         -- ^ when apply check goal
                                 -> (s -> Bool)       -- ^ function used to check if the goal is reached
                                 -> (s -> [(s, Int)]) -- ^ generator of states
                                 -> s                 -- ^ initial state
                                 -> [s]               -- ^ returns list of states
iterativeSearch p ct cg g s = let depths = [1..]
                                  results = map (\d -> searchUntilDepth (Until d) p ct cg g s) depths 
                                  find []      = []
                                  find ([]:xs) = find xs 
                                  find (x:_)   = x
                              in  find results

-- | /searchUntilDepth/ provides a high level interface for a generic Search Tree algorithm which search until
--   the goal is not found
search :: (Eq s, Ord i) => (SNode s -> i)    -- ^ policy to be used
                        -> CheckTime         -- ^ when apply check goal
                        -> (s -> Bool)       -- ^ function used to check if the goal is reached
                        -> (s -> [(s, Int)]) -- ^ generator of states
                        -> s                 -- ^ initial state
                        -> [s]               -- ^ returns list of states
search = searchUntilDepth Forever

-- NOT INFORMED SEARCH --

-- | search with breadth first policy. The root node is expanded first, then all its childrens and so
--   on, It's /complete/ and /optimal/ but very expensive in time and space complexity (/O(b^d)/ with
--   b branching factor and d minimum depth goal)
breadthFirstSearch :: (Eq s) => (s -> Bool)       -- ^ function used to check if the goal is reached
                             -> (s -> [(s, Int)]) -- ^ generator of states
                             -> s                 -- ^ initial state
                             -> [s]               -- ^ returns list of states
breadthFirstSearch = search P.breadthFirstPolicy Generation

-- | generalization of @breadthFirstSearch@ when paths costs are not all equals on the same level
uniformCostSearch :: (Eq s) => (s -> Bool)       -- ^ function used to check if the goal is reached
                            -> (s -> [(s, Int)]) -- ^ generator of states
                            -> s                 -- ^ initial state
                            -> [s]               -- ^ returns list of states
uniformCostSearch = search P.uniformCostPolicy Expansion

-- | search with depth first policy. The deepest node is always expanded first, it is not /complete/ or
--   /optimal/ but keeps only one route at a time in memory so it has a spatial complexity of /O(bm)/ with
--   b branching factor and m max depth in the state space (can be infinite). If in the state graph there are
--   loops then it will loop forever. Time complexity is /O(b^m)/.
depthFirstSearch :: (Eq s) => (s -> Bool)       -- ^ function used to check if the goal is reached
                           -> (s -> [(s, Int)]) -- ^ generator of states
                           -> s                 -- ^ initial state
                           -> [s]               -- ^ returns list of states
depthFirstSearch = search P.depthFirstPolicy Generation

-- INFORMED SEARCH ALGORITHMS --

-- | informed search algorithm which take into account only the value of the heuristic function in order to 
--   choose next state, it is greedy cause it tries to reach the goal as fast as possible.It is not /optimal/
--   and can not be /complete/, but really depends on the heuristic function.
greedyBestFirstSearch :: (Eq s) => (s -> Int)        -- ^ heuristic cost function
                                -> (s -> Bool)       -- ^ function used to check if the goal is reached
                                -> (s -> [(s, Int)]) -- ^ generator of states
                                -> s                 -- ^ initial state
                                -> [s]               -- ^ returns list of states
greedyBestFirstSearch h = search (P.greedyBestFirstPolicy h) Generation

-- | A* search is a informed search algorithm which take into account both cost to reach a node and heuristic function
--   in order to choose next state. It behaves as a @uniformCostSearch@ with a heuristic function
aStarSearch :: (Eq s) => (s -> Int)        -- ^ heuristic cost function
                      -> (s -> Bool)       -- ^ function used to check if the goal is reached
                      -> (s -> [(s, Int)]) -- ^ generator of states
                      -> s                 -- ^ initial state
                      -> [s]               -- ^ returns list of states
aStarSearch h = search (P.aStarPolicy h) Generation
