{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.AI.Search.Fringe.PriorityQueueFringe where

import qualified Data.PQueue.Prio.Min as PQ
import           Data.PQueue.Prio.Min (MinPQueue)
import           Data.AI.Search.Fringe

-- | PQFringe implements Fringe with a Priority Queue
data PQFringe i s = PQFringe !(PQ.MinPQueue i s) (s -> i) -- ^ Priority Queue used

instance (Ord i) => Fringe PQFringe i s where
    
    {-# INLINABLE next #-}
    next (PQFringe queue ord) =
        let minNode  = snd <$> PQ.getMin queue
            newQueue = PQ.deleteMin queue
        in  (,) <$> (Just $! PQFringe newQueue ord) <*> minNode

    {-# INLINABLE insert #-}
    insert (PQFringe q ord) ns =
        let keys     = map ord ns
            newQueue = PQ.fromList (zip keys ns) `PQ.union` q
        in  PQFringe newQueue ord

-- | build a \PQFringe\ to use
empty :: (s -> i) -> PQFringe i s
empty = PQFringe PQ.empty
