{-# LANGUAGE RankNTypes, BangPatterns, FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  AI.Search
-- Copyright   :  Andrea Conti 2019
-- License     :  BSD-3-Clause
-- Maintainer  :  contiandrea96@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Implementation of AI search generic algorithms for graph or tree state 
-- spaces. See @AI.Search.Policies@ for common policies such as breadth first
-- search and so on.
--
-----------------------------------------------------------------------------

module AI.Search 
    ( 
 -- * types   
      MaxDepth(..)

 -- * search functions
    , searchUntil
    , search
    , iterativeSearch

 -- * search state topology
    , tree
    , graph 
    ) where

import qualified Data.AI.Search.SearchRoute.Internal as SR
import           Data.AI.Search.SearchRoute.Internal (SRoute(..))

import qualified Data.AI.Search.SearchNode as SN
import           Data.AI.Search.SearchNode (SNode(..))

import qualified Data.AI.Search.SearchFringe as SF
import           Data.AI.Search.SearchFringe (SFringe)

import           AI.Search.Policies (CheckTime(..), SearchPolicy(..))

import           Control.Monad 
import           Control.Applicative
import           Data.Foldable (toList)
import           Data.Maybe (maybeToList, listToMaybe)

-- ALIASES --

type Topology i s p a = (SRoute (SNode s p a) -> i) -> SF.SFringe i (SRoute (SNode s p a))

tree :: Topology i s p a
tree = SF.emptyTree 

graph :: (Ord s) => Topology i s p a
graph = SF.emptyGraph


-- UTILS --

-- | Goal utility in order to find first goal in a list or nodes
{-# INLINE findGoal #-}
findGoal :: (s -> Bool) -> [s] -> Maybe s
findGoal f = listToMaybe . take 1 . filter f

-- | max depth to search for
data MaxDepth = Forever | Until !Int deriving (Eq, Show)

-- SEARCH --

-- /searchUntil/ search until limited depth
searchUntil :: (Num p, Ord i)
                             => MaxDepth                         -- ^ max depth to search
                             -> Topology i s p a                 -- ^ tree or graph search 
                             -> SearchPolicy s p i a             -- ^ policy to be used
                             -> (s -> Bool)                      -- ^ function used to check if the goal is reached
                             -> (s -> [(a, s, p)])               -- ^ generator of states
                             -> s                                -- ^ initial state
                             -> [a]                              -- ^ returns list of states
searchUntil !md mkFringe (SearchPolicy policy ct) cg g !s = join . maybeToList $ loop initFringe
    where initFringe = SF.insert (mkFringe (policy . SR.takeEnd)) $ map (\(ac, ns, nc) -> SR.singleton $ SNode ns 1 nc ac) (g s)  --[SR.singleton (SNode s 0 0 undefined)]
          
          checkDepth Forever _   = True
          checkDepth (Until d) x = x < d && d > 0

          loop fr = do
            (cFr, routeTail SR.:> currentSNode@(SNode cs cd cc ca) ) <- SF.next fr
            let 
                newStates    = g cs
                nextSNodes   = guard (checkDepth md cd) >> map (\(ac, ns, nc) -> SNode ns (cd+1) (nc+cc) ac) newStates
                genGoal      = guard (ct == Generation && cg cs) >> (Just $! currentSNode)
                expGoal      = guard (ct == Expansion) >> findGoal (cg . state) nextSNodes
            case genGoal <|> expGoal of
                Nothing   -> loop (SF.insert cFr $ map (SR.append $ SR.append routeTail currentSNode ) nextSNodes)
                Just goal -> let (_ SR.:< skipFirst) = routeTail in return $! toList . fmap action $ SR.append skipFirst goal
                            

{-# INLINE search #-}
-- | /search/ implements must generic high level search algorithm
search :: (Num p, Ord i)
                  => Topology i s p a                  -- ^ tree or graph search 
                  -> SearchPolicy s p i a              -- ^ policy to be used
                  -> (s -> Bool)                       -- ^ function used to check if the goal is reached
                  -> (s -> [(a, s, p)])                -- ^ generator of states
                  -> s                                 -- ^ initial state
                  -> [a]                               -- ^ returns list of states
search = searchUntil Forever 


-- | /iterativeSearch/ is equivalent to @searchUntilDepth@ called on increasing depths until the goal is found. Used with
--   a depth first like policy can provide features similar to a search in amplitude (breadth first like policy)
{-# INLINE iterativeSearch #-}
iterativeSearch :: (Num p, Ord i)
                  => Topology i s p a                  -- ^ tree or graph search 
                  -> SearchPolicy s p i a              -- ^ policy to be used
                  -> (s -> Bool)                       -- ^ function used to check if the goal is reached
                  -> (s -> [(a, s, p)])                -- ^ generator of states
                  -> s                                 -- ^ initial state
                  -> [a]                               -- ^ returns list of states
iterativeSearch cls p cg g s =
                              let depths = [1..]
                                  results = map (\d -> searchUntil (Until d) cls p cg g s) depths 
                                  find []      = []
                                  find ([]:xs) = find xs 
                                  find (x:_)   = x
                              in  find results
