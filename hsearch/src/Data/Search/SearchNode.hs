module Data.Search.SearchNode where

-- | Search Node definition, it contains a state, the depth in the
--   search tree and finally the total cost to reach the state
data SNode a = SNode !a
                     {-# UNPACK #-} !Int
                     {-# UNPACK #-} !Int

state :: SNode a -> a
state (SNode s _ _) = s

depth :: SNode a -> Int
depth (SNode _ d _) = d

cost :: SNode a -> Int
cost (SNode _ _ c) = c

instance Eq a => Eq (SNode a) where
    snode1 == snode2 = let s1 = state snode1
                           s2 = state snode2
                       in  s1 == s2

instance Ord a => Ord (SNode a) where
    snode1 <= snode2 = let s1 = state snode1
                           s2 = state snode2
                       in  s1 <= s2
