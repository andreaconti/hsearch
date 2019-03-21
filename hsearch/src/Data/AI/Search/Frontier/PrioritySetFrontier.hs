{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.AI.Search.Frontier.PrioritySetFrontier
    ( PrioritySetFrontier
    , empty
    ) where


import qualified Data.PQueue.Prio.Min as PQ
import           Data.PQueue.Prio.Min (MinPQueue)
import           Data.AI.Search.Frontier
import           Data.List

-- | PQFrontier implements Frontier with a Priority Queue
  
data PrioritySetFrontier i s = PrioritySetFrontier !(PQ.MinPQueue Int s) -- ^ Set of closed nodes
                                                   !(PQ.MinPQueue i s)   -- ^ Priority Queue used
                                                    (s -> i)             -- ^ priority policy used

instance (Ord i) => Frontier (PrioritySetFrontier i) where
    
    {-# INLINABLE next #-}
    next (PrioritySetFrontier closed queue ordering) =
        let minNode  = snd <$> PQ.getMin queue
            newQueue = PQ.deleteMin queue
        in case minNode of
            Nothing -> Nothing
            Just n  -> return $! (PrioritySetFrontier (PQ.insert 0 n closed) newQueue ordering, n)

    {-# INLINABLE insert #-}
    insert (PrioritySetFrontier cl q ord) ns =
        let valids  = ns \\ PQ.elemsU cl
            keys     = map ord valids
            newQueue = PQ.fromList (zip keys valids) `PQ.union` q
        in  PrioritySetFrontier cl newQueue ord

-- | build a \PrioritySetFrontier\ to use
empty :: (Ord i, Eq s) => (s -> i) -> PrioritySetFrontier i s
empty = PrioritySetFrontier PQ.empty PQ.empty
