module Data.AI.Search.SearchNode
    ( SNode(..)
    ) where

-- | search tree's node description, each node contains a state of type
--   `s`, a depth and a cost of type `p`
data SNode s p a = SNode { state  ::                !s   -- ^ state 
                         , depth  :: {-# UNPACK #-} !Int -- ^ depth
                         , cost   ::                !p   -- ^ cost
                         , action ::                !a   -- ^ action
                         }

instance (Eq s) => Eq (SNode s p a) where
    (SNode s1 _ _ _) == (SNode s2 _ _ _) = s1 == s2

instance (Ord s) => Ord (SNode s p a) where
    (SNode s1 _ _ _) <= (SNode s2 _ _ _) = s1 <= s2
