{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.AI.Search.Fringe.PrioritySetFringe
    ( PSFringe
    , empty
    ) where


import qualified Data.PQueue.Prio.Min as PQ
import           Data.PQueue.Prio.Min (MinPQueue)
import           Data.AI.Search.Fringe
import           Data.List

-- | PSFringe implements a fringe which filters in @insert@ calls closed nodes all 
--   items returned at least once by @next@ function
data PSFringe i s = PSFringe !(PQ.MinPQueue Int s) -- ^ Set of closed nodes
                             !(PQ.MinPQueue i s)   -- ^ Priority Queue used

instance Fringe PSFringe where
    
    {-# INLINABLE next #-}
    next (PSFringe closed queue) =
        let minNode  = snd <$> PQ.getMin queue
            newQueue = PQ.deleteMin queue
        in case minNode of
            Nothing -> Nothing
            Just n  -> return $! (PSFringe (PQ.insert 0 n closed) newQueue, n)

    {-# INLINABLE insert #-}
    insert (PSFringe cl q) ord ns =
        let valids  = (nub ns) \\ PQ.elemsU cl
            keys     = map ord valids
            newQueue = PQ.fromList (zip keys valids) `PQ.union` q
        in  PSFringe cl newQueue

instance (Show s, Ord i) => Show (PSFringe i s) where
    show (PSFringe closed queue) = "Closed: "    ++ (show . map snd . PQ.toAscList $ closed)
                                ++ " | Fringe: " ++ (show . map snd . PQ.toAscList $ queue)


-- | build a \PSFringe\ to use
empty :: PSFringe i s
empty = PSFringe PQ.empty PQ.empty
