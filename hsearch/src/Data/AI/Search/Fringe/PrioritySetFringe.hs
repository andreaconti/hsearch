{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.AI.Search.Fringe.PrioritySetFringe
    ( PSFringe
    , empty
    ) where


import qualified Data.PQueue.Prio.Min as PQ
import           Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.Set as S
import           Data.Set (Set)
import           Data.AI.Search.Fringe
import           Data.List
import           Control.Monad

-- | PSFringe implements a fringe which filters in @insert@ calls closed nodes all 
--   items returned at least once by @next@ function
data PSFringe i s = PSFringe !(Set s)              -- ^ Set of closed nodes
                             !(PQ.MinPQueue i s)   -- ^ Priority Queue used
                             (s -> i)              -- ^ ordering function

instance (Ord i, Ord s) => Fringe PSFringe i s where
    
    {-# INLINABLE next #-}
    next (PSFringe closed queue ord) = do
        minNode <- snd <$> PQ.getMin queue
        return $! (PSFringe (S.insert minNode closed) (PQ.deleteMin queue) ord, minNode)

    {-# INLINABLE insert #-}
    insert (PSFringe cl q ord) ns =
        let valids  = filter (\x -> S.notMember x cl) ns 
            keys     = map ord valids
            newQueue = PQ.fromList (zip keys valids) `PQ.union` q
        in  PSFringe cl newQueue ord

instance (Show s, Ord i) => Show (PSFringe i s) where
    show (PSFringe closed queue _) = "Closed: "    ++ (show closed)
                                  ++ " | Fringe: " ++ (show . map snd . PQ.toAscList $ queue)


-- | build a \PSFringe\ to use
empty :: (s -> i) -> PSFringe i s
empty = PSFringe S.empty PQ.empty
