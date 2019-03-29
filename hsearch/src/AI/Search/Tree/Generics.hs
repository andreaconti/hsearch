{-# LANGUAGE RankNTypes, BangPatterns, FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  AI.Search.Tree.Generics
-- Copyright   :  Andrea Conti 2019
-- License     :  BSD-3-Clause
-- Maintainer  :  contiandrea96@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Implementation of AI search generic algorithms for tree search,
-- all functions in @AI.Search.Graph.Algorithms@ rely on this module.
-- Main function is `search`, instead for iterative deepening search 
-- there is `iterativeSearch`
--
-----------------------------------------------------------------------------

module AI.Search.Tree.Generics
    ( module AI.Search.Types
    , SNode(..)
    , iterativeSearch
    , search
    ) where

import qualified Data.AI.Search.SearchRoute.Internal as SR
import           Data.AI.Search.SearchRoute.Internal (SRoute(..))

import qualified Data.AI.Search.SearchNode as SN
import           Data.AI.Search.SearchNode (SNode(..))

import qualified Data.AI.Search.SearchTree as ST
import           Data.AI.Search.SearchTree (STree)

import           AI.Search.Types

import           Control.Monad 
import           Control.Applicative
import           Data.Foldable (toList)
import           Data.Maybe (maybeToList, listToMaybe)

-- UTILS --

-- | Goal utility in order to find first goal in a list or nodes
{-# INLINE findGoal #-}
findGoal :: (s -> Bool) -> [s] -> Maybe s
findGoal f = listToMaybe . take 1 . filter f

-- GENERIC SEARCH --
{-# INLINE search #-}
-- | /genericSearch/ implements must generic high level search algorithm
search :: (Num p, Ord i) => (SNode s p -> a)                 -- ^ result function
                  -> MaxDepth                         -- ^ max depth to search
                  -> (SNode s p -> i)                 -- ^ policy to be used
                  -> CheckTime                        -- ^ when apply check goal
                  -> (s -> Bool)                      -- ^ function used to check if the goal is reached
                  -> (s -> [(s, p)])                  -- ^ generator of states
                  -> s                                -- ^ initial state
                  -> [a]                              -- ^ returns list of states
search rf !md p !ct cg g s = join . maybeToList $ loop initFringe
    where initFringe = ST.singleton (p . SR.takeEnd) $ SR.singleton (SNode s 0 0)
          loop fr = do
            (cFr, routeTail SR.:> currentSNode@(SNode cs cd cc) ) <- ST.minLeaf fr
            let 
                checkDepth Forever _   = True
                checkDepth (Until d) x = x < d && d > 0

                newStates    = g cs
                nextSNodes   = guard (checkDepth md cd) >> map (\(ns, nc) -> SNode ns (cd+1) (nc+cc)) newStates
                genGoal      = guard (ct == Generation && cg cs) >> (Just $! currentSNode)
                expGoal      = guard (ct == Expansion) >> findGoal (cg . state) nextSNodes
            case genGoal <|> expGoal of
                Nothing   -> loop (ST.insert cFr $ map (SR.append $ SR.append routeTail currentSNode) nextSNodes)
                Just goal -> let (_ SR.:< skipFirst) = routeTail in return $! toList . fmap rf $ SR.append skipFirst goal

-- | /iterativeSearch/ is equivalent to @searchUntilDepth@ called on increasing depths until the goal is found. Used with
--   a depth first like policy can provide features similar to a search in amplitude (breadth first like policy)
{-# INLINE iterativeSearch #-}
iterativeSearch :: (Num p, Ord i) => (SNode s p -> a)         -- ^ result function
                           -> (SNode s p -> i)         -- ^ policy to be used
                           -> CheckTime                -- ^ when apply check goal
                           -> (s -> Bool)              -- ^ function used to check if the goal is reached
                           -> (s -> [(s, p)])          -- ^ generator of states
                           -> s                        -- ^ initial state
                           -> [a]                      -- ^ returns list of states
iterativeSearch rf p ct cg g s =
                              let depths = [1..]
                                  results = map (\d -> search rf (Until d) p ct cg g s) depths 
                                  find []      = []
                                  find ([]:xs) = find xs 
                                  find (x:_)   = x
                              in  find results
