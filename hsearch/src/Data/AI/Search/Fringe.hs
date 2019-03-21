{-# LANGUAGE RankNTypes #-}

module Data.AI.Search.Fringe where

{- Definizione della struttura base degli stati, dell'albero degli
 - stati e delle operazioni necessarie ai diversi pattern di ricerca
-}

-- | Fringe definition
class Fringe f where

    -- | retrieve next value from the frontier queue, according to inserting
    -- order
    next   :: (Eq s, Ord i) => f i s -> Maybe (f i s, s)

    -- | insert `s` elements in the Fringe according to ordering policy 
    -- provided by the Fringe building
    insert :: (Eq s, Ord i) => f i s -> (s -> i) -> [s] -> f i s


