{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.AI.Search.Fringe.PQFringe where

import qualified Data.PQueue.Prio.Min as PQ
import           Data.PQueue.Prio.Min (MinPQueue)
import           Data.AI.Search.Fringe

-- | PQFringe implements Fringe with a Priority Queue
data PQFringe i s = PQFringe !(PQ.MinPQueue i s) -- ^ Priority Queue used
                              (s -> i)

instance (Ord i) => Fringe (PQFringe i) where
    
    {-# INLINABLE next #-}
    next (PQFringe queue ordering) =
        let minNode  = snd <$> PQ.getMin queue
            newQueue = PQ.deleteMin queue
        in  (,) <$> (Just $! PQFringe newQueue ordering) <*> minNode

    {-# INLINABLE insert #-}
    insert (PQFringe q ord) ns =
        let keys     = map ord ns
            newQueue = PQ.fromList (zip keys ns) `PQ.union` q
        in  PQFringe newQueue ord

-- | build a \PQFringe\ to use
priorityQueueFringe :: (Ord i, Eq s) => (s -> i) -> PQFringe i s
priorityQueueFringe = PQFringe PQ.empty
