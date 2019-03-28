module Data.AI.Search.SearchTree
    ( STree
    , empty
    , singleton
    , insert
    , minLeaf
    ) where

import qualified Data.PQueue.Prio.Min as PQ
import           Data.PQueue.Prio.Min (MinPQueue)

-- | PQFringe implements Fringe with a Priority Queue
data STree i s = STree !(PQ.MinPQueue i s) (s -> i) -- ^ Priority Queue used

---------------------------------
-- Constructors
---------------------------------

empty :: (s -> i) -> STree i s
empty = STree PQ.empty

singleton :: (s -> i) -> s -> STree i s
singleton f s = STree (PQ.singleton (f s) s) f

insert :: (Ord i) => STree i s -> [s] -> STree i s
insert (STree q ord) ns =
    let keys     = map ord ns
        newQueue = PQ.fromList (zip keys ns) `PQ.union` q
    in  STree newQueue ord

---------------------------------
-- Deconstructors
---------------------------------

minLeaf :: (Ord i) => STree i s -> Maybe (STree i s, s)
minLeaf (STree queue ord) =
        let minNode  = snd <$> PQ.getMin queue
            newQueue = PQ.deleteMin queue
        in  (,) <$> (Just $! STree newQueue ord) <*> minNode
