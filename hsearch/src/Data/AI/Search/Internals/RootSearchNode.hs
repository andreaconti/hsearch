module Data.AI.Search.Internals.RootSearchNode 
    ( RSNode(..)
    , toList
    , stateUnsafe
    )
where

-- | Search Node definition, it contains a state, the depth in the
--   search tree and finally the total cost to reach the state
data RSNode a p = Root
              | RSNode !(RSNode a p) !a {-# UNPACK #-} !Int !p


toList' :: [a] -> RSNode a p -> [a]
toList' acc Root             = acc
toList' acc (RSNode r s _ _) = toList' (s : acc) r 

toList :: RSNode a p -> [a]
toList = toList' []

stateUnsafe :: RSNode a p -> a
stateUnsafe (RSNode _ s _ _) = s

instance Eq a => Eq (RSNode a p) where
    (RSNode _ s1 _ _) == (RSNode _ s2 _ _ ) = s1 == s2

instance Ord a => Ord (RSNode a p) where
    (RSNode _ s1 _ _) <= (RSNode _ s2 _ _ ) = s1 <= s2
