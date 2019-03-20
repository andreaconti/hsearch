{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Search.Frontier.SkipEqPQFrontier
    ( SKPFrontier
    , skipPriorityFrontier
    ) where


import qualified Data.PQueue.Prio.Min as PQ
import           Data.PQueue.Prio.Min (MinPQueue)
import           Data.Search.Frontier
import Data.List

-- | PQFrontier implements Frontier with a Priority Queue
  
data SKPFrontier i s = SKPFrontier !(PQ.MinPQueue Int s) -- ^ Set of closed nodes
                                   !(PQ.MinPQueue i s)   -- ^ Priority Queue used
                                    (s -> i)             -- ^ priority policy used

instance (Ord i) => Frontier (SKPFrontier i) where
    
    {-# INLINABLE next #-}
    next (SKPFrontier closed queue ordering) =
        let minNode  = snd <$> PQ.getMin queue
            newQueue = PQ.deleteMin queue
        in case minNode of
            Nothing -> Nothing
            Just n  -> return $! (SKPFrontier (PQ.insert 0 n closed) newQueue ordering, n)

    {-# INLINABLE insert #-}
    insert (SKPFrontier cl q ord) ns =
        let valids  = ns \\ (PQ.elemsU cl)
            keys     = map ord valids
            newQueue = PQ.fromList (zip keys valids) `PQ.union` q
        in  SKPFrontier cl newQueue ord

-- | build a \SKPFrontier\ to use
skipPriorityFrontier :: (Ord i, Eq s) => (s -> i) -> SKPFrontier i s
skipPriorityFrontier = SKPFrontier (PQ.empty) (PQ.empty)
