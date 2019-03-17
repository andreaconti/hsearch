{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

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
    ( search
    ) where

import qualified Data.Search.SearchNode as SN
import           Data.Search.SearchNode (SNode(..))
import qualified Data.Search.Impl.RootSearchNode as RN
import           Data.Search.Impl.RootSearchNode (RSNode(..))
import           Data.Search.Frontier (Frontier, next, insert)
import qualified Data.Search.Frontier.PQFrontier as PQ

import           Search.Policies (CheckTime(..))

import           Control.Monad 
import           Control.Applicative
import           Data.Maybe (maybeToList, listToMaybe)

-- | Goal utility in order to find first goal in a list o nodes
findGoal :: (s -> Bool) -> [s] -> Maybe s
findGoal f = listToMaybe . take 1 . filter f

-- | 'search' function provides a high level customizable way to specific a search algorithm
search :: (Eq s, Ord i) => (SNode s -> i)    -- ^ policy to be used
                        -> CheckTime         -- ^ when apply policy
                        -> (s -> Bool)       -- ^ function used to check if the goal is reached
                        -> (s -> [(s, Int)]) -- ^ generator of states
                        -> s                 -- ^ initial state
                        -> [s]               -- ^ returns list of states
search p ct cg g s = join . maybeToList $ loop initFrontier
    where initFrontier = insert (PQ.priorityQueueFrontier (\(RSNode _ x) -> p x)) [RSNode Root (SN.SNode s 0 0)]
          loop fr = do
            (cFr, rsNode@(RSNode _ currentNode@(SNode s d c))) <- next fr
            let 
                newSNodes  = map (\(ns, nc) -> SNode ns (d+1) (nc+c)) $ g s
                genGoal    = guard (ct == Generation) >> findGoal cg [s]
                expGoal    = guard (ct == Expansion) >> findGoal cg (map SN.state newSNodes)
            case genGoal <|> expGoal of
                Nothing -> loop (insert cFr $ map (RSNode rsNode) newSNodes)
                goal    -> return $ map SN.state . RN.rsNodeToList $ rsNode

