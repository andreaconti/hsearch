{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Search 
    ( search
    , CheckTime
    ) where

import qualified Data.Search.SearchNode as SN
import           Data.Search.SearchNode (SNode(..))
import qualified Data.Search.Impl.RootSearchNode as RN
import           Data.Search.Impl.RootSearchNode (RSNode(..))
import           Data.Search.Frontier (Frontier, next, insert)
import qualified Data.Search.Frontier.PQFrontier as PQ

import           Control.Monad 
import           Control.Applicative
import           Data.Maybe (maybeToList, listToMaybe)


-- | To specify if the goal reached check must be done at node generation
-- or at goal expansion
data CheckTime = Generation | Expansion
    deriving Eq

-- | Goal utility in order to find first goal in a list o nodes
findGoal :: (s -> Bool) -> [s] -> Maybe s
findGoal f = listToMaybe . take 1 . filter f

-- | search doc TODO
search :: (Eq s, Ord i) => (s -> [(s, Int)]) -- generator
                        -> (s -> Bool)       -- checkGoal
                        -> (SNode s -> i)    -- policy
                        -> CheckTime         -- when apply policy
                        -> s                 -- initial state
                        -> [s]               -- returns list of states
search g cg p ct s = join . maybeToList $ loop initFrontier
    where initFrontier = insert (PQ.priorityQueueFrontier (\(RSNode _ x) -> p x)) [RSNode Root (SN.SNode s 0 0)]
          loop fr = do
            (cFr, rsNode@(RSNode _ currentNode@(SNode s d c))) <- next fr
            let 
                newSNodes  = map (\(ns, nc) -> SNode ns (d+1) (nc+c)) $ g s
                genGoal    = guard (ct == Generation) >> findGoal cg [s]
                expGoal    = guard (ct == Expansion) >> findGoal cg (map SN.state newSNodes)
            case genGoal <|> expGoal of
                Nothing -> loop (insert cFr $ map (RSNode rsNode) newSNodes)
                goal    -> return $ RN.rsNodeToList rsNode
