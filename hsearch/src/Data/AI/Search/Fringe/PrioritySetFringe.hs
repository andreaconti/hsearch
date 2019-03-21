{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.AI.Search.Fringe.PrioritySetFringe
    ( PrioritySetFringe
    , empty
    ) where


import qualified Data.PQueue.Prio.Min as PQ
import           Data.PQueue.Prio.Min (MinPQueue)
import           Data.AI.Search.Fringe
import           Data.List

-- | PQFringe implements Fringe with a Priority Queue
  
data PrioritySetFringe i s = PrioritySetFringe !(PQ.MinPQueue Int s) -- ^ Set of closed nodes
                                               !(PQ.MinPQueue i s)   -- ^ Priority Queue used

instance Fringe PrioritySetFringe where
    
    {-# INLINABLE next #-}
    next (PrioritySetFringe closed queue) =
        let minNode  = snd <$> PQ.getMin queue
            newQueue = PQ.deleteMin queue
        in case minNode of
            Nothing -> Nothing
            Just n  -> return $! (PrioritySetFringe (PQ.insert 0 n closed) newQueue, n)

    {-# INLINABLE insert #-}
    insert (PrioritySetFringe cl q) ord ns =
        let valids  = ns \\ PQ.elemsU cl
            keys     = map ord valids
            newQueue = PQ.fromList (zip keys valids) `PQ.union` q
        in  PrioritySetFringe cl newQueue

-- | build a \PrioritySetFringe\ to use
empty :: PrioritySetFringe i s
empty = PrioritySetFringe PQ.empty PQ.empty
