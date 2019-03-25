{-# LANGUAGE RankNTypes #-}

module Data.AI.Search.Fringe where


-- | Fringe typeclass definition, it enables to store with a specific order policy
--   elements, retrieved with @next@. This simple typeclass enables to change fringe
--   implementation in order to customize behavior. Different instances are under
--   @Data.AI.Search.Fringe@ 
class Fringe f where

    -- | retrieve next value from the frontier queue, according to ordering provided
    next   :: (Eq s, Ord i) => f i s -> Maybe (f i s, s)

    -- | insert `s` elements in the Fringe according to ordering policy provided, different
    --   calls to `insert` must be use same ordering policy
    insert :: (Eq s, Ord i) => f i s -> (s -> i) -> [s] -> f i s


