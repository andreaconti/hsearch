module Data.AI.Search.SearchGraph
    ( SGraph
    , empty
    , singleton
    , insert
    , minNode
    ) where


import qualified Data.PQueue.Prio.Min as PQ
import           Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.Set as Set
import           Data.Set (Set)
import           Control.Monad

-- | SGraph implements a fringe which filters in @insert@ calls closed nodes all 
--   items returned at least once by @next@ function
data SGraph i s = SGraph !(Set s)              -- ^ Set of closed nodes
                                   !(PQ.MinPQueue i s)   -- ^ Priority Queue used
                                   (s -> i)              -- ^ ordering function

----------------------------------------
-- Instances
----------------------------------------

instance (Show s, Ord i) => Show (SGraph i s) where
    show (SGraph closed queue _) = "Closed: "    ++ (show closed)
                                  ++ " | Fringe: " ++ (show . map snd . PQ.toAscList $ queue)

---------------------------------
-- Constructors
---------------------------------

empty :: (s -> i) -> SGraph i s
empty = SGraph Set.empty PQ.empty

singleton :: (s -> i) -> s -> SGraph i s
singleton f s = SGraph Set.empty (PQ.singleton (f s) s) f
    
{-# INLINABLE insert #-}
insert :: (Ord i, Ord s) => SGraph i s -> [s] -> SGraph i s
insert (SGraph cl q ord) ns =
    let valids  = filter (\x -> Set.notMember x cl) ns 
        keys     = map ord valids
        newQueue = PQ.fromList (zip keys valids) `PQ.union` q
    in  SGraph cl newQueue ord

----------------------------------
-- Decostructors
----------------------------------

{-# INLINABLE minNode #-}
minNode :: (Ord i, Ord s) => SGraph i s -> Maybe (SGraph i s, s)
minNode (SGraph closed queue ord) = do
    minNode <- snd <$> PQ.getMin queue
    return $! (SGraph (Set.insert minNode closed) (PQ.deleteMin queue) ord, minNode)
