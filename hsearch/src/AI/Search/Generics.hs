{-# LANGUAGE RankNTypes, BangPatterns, FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Search
-- Copyright   :  Andrea Conti 2019
-- License     :  BSD-3-Clause
-- Maintainer  :  contiandrea96@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Implementation of AI search generic algorithms, all functions in 
-- @AI.Search.Algorithms@ rely on this module.  
--
-----------------------------------------------------------------------------

module AI.Search.Generics
    ( MaxDepth(..)
    , CheckTime(..)
    , SNode(..)
    , genericSearch
    , searchUntilDepth
    , iterativeSearch
    , search
    ) where

import qualified Data.AI.Search.Internals.RootSearchNode as RSN
import           Data.AI.Search.Internals.RootSearchNode (RSNode(..))

import qualified Data.AI.Search.SearchRoute.Internal as SR
import           Data.AI.Search.SearchRoute.Internal (SRoute(..))

import qualified Data.AI.Search.Fringe.PriorityQueueFringe as PQ
import           Data.AI.Search.Fringe (Fringe, next, insert)

import           Control.Monad 
import           Control.Applicative
import           Data.Foldable (toList)
import           Data.Maybe (maybeToList, listToMaybe)

-- TYPES --

-- | max depth to search for
data MaxDepth = Forever | Until !Int deriving (Eq, Show)

-- | search tree's node description, each node contains a state of type
--   `s`, a depth and a cost of type `p`
data SNode s p = SNode { state ::                !s   -- ^ state 
                       , depth :: {-# UNPACK #-} !Int -- ^ depth
                       , cost  ::                !p   -- ^ cost
                       }

instance (Eq s) => Eq (SNode s p) where
    (SNode s1 _ _) == (SNode s2 _ _) = s1 == s2

instance (Ord s) => Ord (SNode s p) where
    (SNode s1 _ _) <= (SNode s2 _ _) = s1 <= s2

-- | To check if the target has been reached at Generation time or Expansion time 
data CheckTime = Generation | Expansion
    deriving (Eq, Show)

-- UTILS --

-- | Goal utility in order to find first goal in a list or nodes
{-# INLINE findGoal #-}
findGoal :: (s -> Bool) -> [s] -> Maybe s
findGoal f = listToMaybe . take 1 . filter f

-- GENERIC SEARCH --
{-# INLINE genericSearch #-}
-- | /genericSearch/ implements must generic high level search algorithm
genericSearch :: (Fringe f i (SRoute (SNode s p)), Num p) => f i (SRoute (SNode s p)) -- ^ frontier used
                                                  -> (SNode s p -> a)                 -- ^ result function
                                                  -> MaxDepth                         -- ^ max depth to search
                                                  -> CheckTime                        -- ^ when apply check goal
                                                  -> (s -> Bool)                      -- ^ function used to check if the goal is reached
                                                  -> (s -> [(s, p)])                  -- ^ generator of states
                                                  -> s                                -- ^ initial state
                                                  -> [a]                              -- ^ returns list of states
genericSearch frontier rf !md !ct cg g s = join . maybeToList $ loop initFringe
    where initFringe = insert frontier [SR.singleton (SNode s 0 0)]
          loop fr = do
            (cFr, routeTail SR.:> currentSNode@(SNode cs cd cc) ) <- next fr
            let 
                checkDepth Forever _   = True
                checkDepth (Until d) x = x < d && d > 0

                newStates    = g cs
                nextSNodes   = guard (checkDepth md cd) >> map (\(ns, nc) -> SNode ns (cd+1) (nc+cc)) newStates
                genGoal      = guard (ct == Generation && cg cs) >> (Just $! currentSNode)
                expGoal      = guard (ct == Expansion) >> findGoal (cg . state) nextSNodes
            case genGoal <|> expGoal of
                Nothing   -> loop (insert cFr $ map (SR.append routeTail) nextSNodes)
                Just goal -> return $! toList . fmap rf $ SR.append routeTail goal

-- | /searchUntilDepth/ provides a high level interface for a generic Search Tree algorithm with custom max
--   depth to search for
{-# INLINE searchUntilDepth #-}
searchUntilDepth :: (Eq s, Ord i, Num p) => MaxDepth     -- ^ max depth to search
                                  -> (SNode s p -> i)    -- ^ policy to be used
                                  -> CheckTime           -- ^ when apply check goal
                                  -> (s -> Bool)         -- ^ function used to check if the goal is reached
                                  -> (s -> [(s, p)])     -- ^ generator of states
                                  -> s                   -- ^ initial state
                                  -> [s]                 -- ^ returns list of states
searchUntilDepth md p = genericSearch (PQ.empty $ p . SR.takeEnd) state md

-- | /iterativeSearch/ is equivalent to @searchUntilDepth@ called on increasing depths until the goal is found. Used with
--   a depth first like policy can provide features similar to a search in amplitude (breadth first like policy)
{-# INLINE iterativeSearch #-}
iterativeSearch :: (Eq s, Ord i, Num p) => (SNode s p -> i)  -- ^ policy to be used
                                 -> CheckTime                -- ^ when apply check goal
                                 -> (s -> Bool)              -- ^ function used to check if the goal is reached
                                 -> (s -> [(s, p)])          -- ^ generator of states
                                 -> s                        -- ^ initial state
                                 -> [s]                      -- ^ returns list of states
iterativeSearch p ct cg g s = let depths = [1..]
                                  results = map (\d -> searchUntilDepth (Until d) p ct cg g s) depths 
                                  find []      = []
                                  find ([]:xs) = find xs 
                                  find (x:_)   = x
                              in  find results

-- | /searchUntilDepth/ provides a high level interface for a generic Search Tree algorithm which search until
--   the goal is not found
{-# INLINE search #-}
search :: (Eq s, Ord i, Num p) => (SNode s p -> i)  -- ^ policy to be used
                        -> CheckTime                -- ^ when apply check goal
                        -> (s -> Bool)              -- ^ function used to check if the goal is reached
                        -> (s -> [(s, p)])          -- ^ generator of states
                        -> s                        -- ^ initial state
                        -> [s]                      -- ^ returns list of states
search = searchUntilDepth Forever
