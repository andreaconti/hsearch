{-# LANGUAGE RankNTypes #-}

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
data MaxDepth = Forever | Until Int deriving (Eq, Show)

-- | Goal utility in order to find first goal in a list o nodes
findGoal :: (s -> Bool) -> [s] -> Maybe s
findGoal f = listToMaybe . take 1 . filter f

-- | 'searchUntilDepth' function provides a high level customizable way to specific a search algorithm
searchUntilDepth :: (Eq s, Ord i) => MaxDepth          -- ^ max depth to search
                                  -> (SNode s -> i)    -- ^ policy to be used
                                  -> CheckTime         -- ^ when apply check goal
                                  -> (s -> Bool)       -- ^ function used to check if the goal is reached
                                  -> (s -> [(s, Int)]) -- ^ generator of states
                                  -> s                 -- ^ initial state
                                  -> [s]               -- ^ returns list of states
searchUntilDepth md p ct cg g s = join . maybeToList $ loop initFrontier
    where initFrontier = insert (PQ.priorityQueueFrontier (\(RSNode _ x) -> p x)) [RSNode Root (SN.SNode s 0 0)]
          loop fr = do
            (cFr, rsNode@(RSNode _ currentNode@(SNode cs cd cc))) <- next fr
            let 
                checkDepth Forever _   = True
                checkDepth (Until d) x = x < d && d > 0

                newSNodes    = guard (checkDepth md cd) >> map (\(ns, nc) -> SNode ns (cd+1) (nc+cc)) (g cs)
                genGoal      = guard (ct == Generation) >> findGoal cg [cs]
                expGoal      = guard (ct == Expansion) >> findGoal cg (map SN.state newSNodes)
            case genGoal <|> expGoal of
                Nothing   -> loop (insert cFr $ map (RSNode rsNode) newSNodes)
                Just goal -> return $ map SN.state . RN.rsNodeToList $ rsNode

-- | 'iterativeSearch' implements a search using a progressive depth until result is found
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

-- | 'search' function provides a high level customizable way to specific a search algorithm
search :: (Eq s, Ord i) => (SNode s -> i)    -- ^ policy to be used
                        -> CheckTime         -- ^ when apply check goal
                        -> (s -> Bool)       -- ^ function used to check if the goal is reached
                        -> (s -> [(s, Int)]) -- ^ generator of states
                        -> s                 -- ^ initial state
                        -> [s]               -- ^ returns list of states
search = searchUntilDepth Forever

-- | 'breadth-first search' implementation
breadthFirstSearch :: (Eq s) => (s -> Bool)       -- ^ function used to check if the goal is reached
                             -> (s -> [(s, Int)]) -- ^ generator of states
                             -> s                 -- ^ initial state
                             -> [s]               -- ^ returns list of states
breadthFirstSearch = search P.breadthFirstPolicy Generation

-- | 'uniform-cost search' implementation
uniformCostSearch :: (Eq s) => (s -> Bool)       -- ^ function used to check if the goal is reached
                            -> (s -> [(s, Int)]) -- ^ generator of states
                            -> s                 -- ^ initial state
                            -> [s]               -- ^ returns list of states
uniformCostSearch = search P.uniformCostPolicy Expansion

-- | 'depth-first search'  implementation
depthFirstSearch :: (Eq s) => (s -> Bool)       -- ^ function used to check if the goal is reached
                           -> (s -> [(s, Int)]) -- ^ generator of states
                           -> s                 -- ^ initial state
                           -> [s]               -- ^ returns list of states
depthFirstSearch = search P.depthFirstPolicy Generation
