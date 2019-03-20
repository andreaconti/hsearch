module Search.Policies 
    ( breadthFirstPolicy
    , uniformCostPolicy
    , depthFirstPolicy
    , greedyBestFirstPolicy
    , aStarPolicy
    ) where

import           Data.Function (on)
import qualified Search.Generics as SN
import           Search.Generics (SNode)

-- NOT INFORMED POLICIES --

-- | Breadth-First Search policy implementation, returns the depth
{-# INLINE breadthFirstPolicy #-}
breadthFirstPolicy :: (Eq s) => SNode s -> Int
breadthFirstPolicy = SN.depth

-- | Uniform-Cost Search policy implementation
{-# INLINE uniformCostPolicy #-}
uniformCostPolicy :: (Eq s) => SNode s -> Int
uniformCostPolicy = SN.cost

-- | Depth-First Search policy implementation
{-# INLINE depthFirstPolicy #-}
depthFirstPolicy :: (Eq s) => SNode s -> Int
depthFirstPolicy n = - (SN.depth n)

-- INFORMED (HEURISTIC) SEARCH --

{-# INLINE greedyBestFirstPolicy #-}
greedyBestFirstPolicy :: (s -> Int) -> SNode s -> Int
greedyBestFirstPolicy h node = h (SN.state node)

{-# INLINE aStarPolicy #-}
aStarPolicy :: (s -> Int) -> SNode s -> Int
aStarPolicy h node = h (SN.state node) + SN.cost node

