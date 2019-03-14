{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Search.Frontier.PQFrontier where

import qualified Data.PQueue.Prio.Min as PQ
import           Data.PQueue.Prio.Min (MinPQueue)
import           Data.Search.Frontier
import qualified Data.Search.SearchNode as SN
import           Data.Search.SearchNode (SNode)

-- | PQFrontier implements Frontier with a Priority Queue
data PQFrontier i s = PQFrontier
    { getPQueue :: PQ.MinPQueue i s
    , ordering  :: s -> i
    }

instance (Ord i) => Frontier (PQFrontier i) where
    
    next frontier =
        let queue    = getPQueue frontier
            minNode  = snd <$> PQ.getMin queue
            newQueue = PQ.deleteMin queue
        in  (,) <$> (Just $ PQFrontier newQueue (ordering frontier)) <*> minNode

    insert (PQFrontier q ord) ns =
        let keys     = map ord ns
            newQueue = PQ.fromList (zip keys ns) `PQ.union` q
        in  PQFrontier newQueue ord

-- | build a \PQFrontier\ to use
priorityQueueFrontier :: (Ord i, Eq s) => (s -> i) -> PQFrontier i s
priorityQueueFrontier = PQFrontier PQ.empty
