{-# LANGUAGE MultiParamTypeClasses, GADTs, FlexibleInstances #-}

module Data.AI.Search.SearchFringe.Internal where


import qualified Data.PQueue.Prio.Min as PQ
import           Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.Set as Set
import           Data.Set (Set)
import           Control.Monad

-- | SFringe implements a fringe which filters in @insert@ calls closed nodes all 
--   items returned at least once by @next@ function
data SFringe i s where
    SFringe :: ClosedNodes f s => f s               -- ^ select tree or graph mode
                               -> PQ.MinPQueue i s  -- ^ Priority Queue used
                               -> (s -> i)          -- ^ ordering function
                               -> SFringe i s

----------------------------------------
-- Instances
----------------------------------------

instance (Show s, Ord i) => Show (SFringe i s) where
    show (SFringe closed queue _) = show . map snd . PQ.toAscList $ queue

----------------------------------------
-- Private Datas
----------------------------------------

class ClosedNodes f s where
    
    (<<) :: f s -> s -> f s

    isClosed :: f s -> s -> Bool

    isNotClosed :: f s -> s -> Bool
    isNotClosed f v = not . isClosed f $ v
    

instance (Ord s) => ClosedNodes Set s where
    (<<) set x = Set.insert x set
    isClosed set x = Set.member x set

graph :: (Ord s) => Set s
graph = Set.empty

data DoNothing s = DoNothing

instance ClosedNodes DoNothing s where
    (<<) set _ = set
    isClosed _ _ = False

tree :: DoNothing s
tree = DoNothing

---------------------------------
-- Constructors
---------------------------------

{-# INLINABLE empty #-}
empty :: (ClosedNodes f s) => f s -> (s -> i) -> SFringe i s
empty cl = SFringe cl PQ.empty

{-# INLINABLE singleton #-}
singleton :: (ClosedNodes f s) => f s -> (s -> i) -> s -> SFringe i s
singleton cl f s = SFringe cl (PQ.singleton (f s) s) f
    
{-# INLINABLE insert #-}
insert :: (Ord i) => SFringe i s -> [s] -> SFringe i s
insert (SFringe cl q ord) ns =
    let valids   = filter (\x -> isNotClosed cl x) ns 
        keys     = map ord valids
        newQueue = PQ.fromList (zip keys valids) `PQ.union` q
    in  SFringe cl newQueue ord

----------------------------------
-- Decostructors
----------------------------------

{-# INLINABLE next #-}
next :: (Ord i) => SFringe i s -> Maybe (SFringe i s, s)
next (SFringe closed queue ord) = do
    minNode <- snd <$> PQ.getMin queue
    return $! (SFringe (closed << minNode) (PQ.deleteMin queue) ord, minNode)


