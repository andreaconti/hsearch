module Data.Search.SearchNode where

-- | Search Node definition, it contains a state, the depth in the
--   search tree and finally the total cost to reach the state
data SNode a = Root
             | SNode !(SNode a)
                     !a
                     {-# UNPACK #-} !Int
                     {-# UNPACK #-} !Int

state :: SNode a -> a
state (SNode _ s _ _) = s

depth :: SNode a -> Int
depth (SNode _ _ d _) = d

cost :: SNode a -> Int
cost (SNode _ _ _ c) = c

toList' :: [a] -> SNode a -> [a]
toList' acc Root            = acc
toList' acc (SNode r s _ _) = toList' (s : acc) r 

toList :: SNode a -> [a]
toList = toList' []

instance Eq a => Eq (SNode a) where
    snode1 == snode2 = let s1 = state snode1
                           s2 = state snode2
                       in  s1 == s2

instance Ord a => Ord (SNode a) where
    snode1 <= snode2 = let s1 = state snode1
                           s2 = state snode2
                       in  s1 <= s2
