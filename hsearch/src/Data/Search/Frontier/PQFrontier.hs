{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Search.Frontier.PQFrontier where

import qualified Data.PQueue.Prio.Min as PQ
import           Data.PQueue.Prio.Min (MinPQueue)
import           Data.Search.Frontier

-- | PQFrontier implements Frontier with a Priority Queue
data PQFrontier i s = PQFrontier {-# UNPACK #-} !(PQ.MinPQueue i s) -- ^ Priority Queue used
                                                 (s -> i)           -- ^ priority policy used

instance (Ord i) => Frontier (PQFrontier i) where
    
    {-# INLINABLE next #-}
    next (PQFrontier queue ordering) =
        let minNode  = snd <$> PQ.getMin queue
            newQueue = PQ.deleteMin queue
        in  (,) <$> (Just $! PQFrontier newQueue ordering) <*> minNode

    {-# INLINABLE insert #-}
    insert (PQFrontier q ord) ns =
        let keys     = map ord ns
            newQueue = PQ.fromList (zip keys ns) `PQ.union` q
        in  PQFrontier newQueue ord

-- | build a \PQFrontier\ to use
priorityQueueFrontier :: (Ord i, Eq s) => (s -> i) -> PQFrontier i s
priorityQueueFrontier = PQFrontier PQ.empty
