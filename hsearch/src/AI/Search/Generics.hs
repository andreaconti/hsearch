{-# LANGUAGE RankNTypes, BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Search
-- Copyright   :  Andrea Conti 2019
-- License     :  BSD-3-Clause
-- Maintainer  :  contiandrea96@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Implementation of AI search algorithms.
-- TODO
--
--
-----------------------------------------------------------------------------

module AI.Search.Generics
    ( MaxDepth(..)
    , CheckTime(..)
    , SNode
    , state, cost, depth
    , genericSearch
    , searchUntilDepth
    , iterativeSearch
    , search
    ) where

import qualified Data.AI.Search.Internals.RootSearchNode as RSN
import           Data.AI.Search.Internals.RootSearchNode (RSNode(..))

import qualified Data.AI.Search.Fringe.PriorityQueueFringe as PQ
import           Data.AI.Search.Fringe (Fringe, next, insert)

import           Control.Monad 
import           Control.Applicative
import           Data.Maybe (maybeToList, listToMaybe)

-- TYPES --

-- | max depth to search for
data MaxDepth = Forever | Until !Int deriving (Eq, Show)

newtype SNode s p = SNode { getRSNode :: RSNode s p } deriving (Eq)

{-# INLINE state #-}
state :: SNode s p -> s
state (SNode (RSNode _ s _ _)) = s

{-# INLINE depth #-}
depth :: SNode s p -> Int
depth (SNode (RSNode _ _ d _)) = d

{-# INLINE cost #-}
cost :: SNode s p -> p
cost (SNode (RSNode _ _ _ c)) = c

-- | To specify if the goal reached check must be done at node generation
-- or at goal expansion
data CheckTime = Generation | Expansion
    deriving Eq

-- UTILS --

-- | Goal utility in order to find first goal in a list o nodes
{-# INLINE findGoal #-}
findGoal :: (s -> Bool) -> [s] -> Maybe s
findGoal f = listToMaybe . take 1 . filter f

{-# INLINE toSNodeList' #-}
toSNodeList' :: [SNode a p] -> RSNode a p -> [SNode a p]
toSNodeList' acc Root             = acc
toSNodeList' acc rsnode@(RSNode r _ _ _) = toSNodeList' (SNode rsnode : acc) r 

{-# INLINE toSNodeList #-}
toSNodeList :: RSNode a p -> [SNode a p]
toSNodeList = toSNodeList' []

-- GENERIC SEARCH --

{-# INLINE genericSearch #-}
genericSearch :: (Fringe f, Eq s, Num p, Ord i) => f i (RSNode s p)        -- ^ frontier used
                                                -> (SNode s p -> a)        -- ^ result function
                                                -> MaxDepth                -- ^ max depth to search
                                                -> (SNode s p -> i)        -- ^ policy to be used
                                                -> CheckTime               -- ^ when apply check goal
                                                -> (s -> Bool)             -- ^ function used to check if the goal is reached
                                                -> (s -> [(s, p)])         -- ^ generator of states
                                                -> s                       -- ^ initial state
                                                -> [a]                     -- ^ returns list of states
genericSearch frontier rf !md p !ct cg g s = join . maybeToList $ loop initFringe
    where initFringe = insert frontier (p . SNode) [RSN.RSNode RSN.Root s 0 0]
          loop fr = do
            (cFr, currentNode@(RSNode _ cs cd cc)) <- next fr
            let 
                checkDepth Forever _   = True
                checkDepth (Until d) x = x < d && d > 0

                newStates    = g cs
                newRSNodes   = guard (checkDepth md cd) >> map (\(ns, nc) -> RSNode currentNode ns (cd+1) (nc+cc)) newStates
                genGoal      = guard (ct == Generation && cg cs) >> (Just $! currentNode)
                expGoal      = guard (ct == Expansion) >> findGoal (cg . RSN.stateUnsafe) newRSNodes
            case genGoal <|> expGoal of
                Nothing   -> loop (insert cFr (p . SNode) newRSNodes)
                Just goal -> return $! map rf (toSNodeList goal)


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
searchUntilDepth = genericSearch PQ.empty state

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
