module Data.Search.Internals.RootSearchNode 
    ( RSNode(..)
    , toList
    , stateUnsafe
    )
where

-- | Search Node definition, it contains a state, the depth in the
--   search tree and finally the total cost to reach the state
data RSNode a = Root
             | RSNode !(RSNode a)
                     !a
                     {-# UNPACK #-} !Int
                     {-# UNPACK #-} !Int


toList' :: [a] -> RSNode a -> [a]
toList' acc Root             = acc
toList' acc (RSNode r s _ _) = toList' (s : acc) r 

toList :: RSNode a -> [a]
toList = toList' []

stateUnsafe :: RSNode a -> a
stateUnsafe (RSNode _ s _ _) = s

instance Eq a => Eq (RSNode a) where
    (RSNode _ s1 _ _) == (RSNode _ s2 _ _ ) = s1 == s2

instance Ord a => Ord (RSNode a) where
    (RSNode _ s1 _ _) <= (RSNode _ s2 _ _ ) = s1 <= s2
