{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.AI.Search.Fringe.PriorityQueueFringe where

import qualified Data.PQueue.Prio.Min as PQ
import           Data.PQueue.Prio.Min (MinPQueue)
import           Data.AI.Search.Fringe

-- | PQFringe implements Fringe with a Priority Queue
newtype PQFringe i s = PQFringe (PQ.MinPQueue i s) -- ^ Priority Queue used

instance Fringe PQFringe where
    
    {-# INLINABLE next #-}
    next (PQFringe queue) =
        let minNode  = snd <$> PQ.getMin queue
            newQueue = PQ.deleteMin queue
        in  (,) <$> (Just $! PQFringe newQueue) <*> minNode

    {-# INLINABLE insert #-}
    insert (PQFringe q) ord ns =
        let keys     = map ord ns
            newQueue = PQ.fromList (zip keys ns) `PQ.union` q
        in  PQFringe newQueue

-- | build a \PQFringe\ to use
empty :: PQFringe i s
empty = PQFringe PQ.empty
