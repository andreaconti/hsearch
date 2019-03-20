{-# LANGUAGE RankNTypes #-}

module Data.Search.Frontier where

{- Definizione della struttura base degli stati, dell'albero degli
 - stati e delle operazioni necessarie ai diversi pattern di ricerca
-}

-- | Frontier definition
class Frontier f where

    -- | retrieve next value from the frontier queue, according to inserting
    -- order
    next   :: f s -> Maybe (f s, s)

    -- | insert `s` elements in the Frontier according to ordering policy 
    -- provided by the Frontier building
    insert :: f s -> [s] -> f s


