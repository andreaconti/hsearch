module Data.Search.SearchNode where

-- DEFINIZIONE NODO
data SNode a = SNode
        { state :: a
        , depth :: Int
        , cost  :: Int
        }

instance Eq a => Eq (SNode a) where
    snode1 == snode2 = let s1 = state snode1
                           s2 = state snode2
                       in  s1 == s2

instance Ord a => Ord (SNode a) where
    snode1 <= snode2 = let s1 = state snode1
                           s2 = state snode2
                       in  s1 <= s2
