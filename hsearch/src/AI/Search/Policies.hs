module AI.Search.Policies 
    ( breadthFirstPolicy
    , uniformCostPolicy
    , depthFirstPolicy
    , greedyBestFirstPolicy
    , aStarPolicy
    ) where

import           Data.Function (on)
import qualified Data.AI.Search.SearchNode as SN
import           Data.AI.Search.SearchNode (SNode)

-- NOT INFORMED POLICIES --

-- | Breadth-First Search policy implementation, returns the depth
{-# INLINE breadthFirstPolicy #-}
breadthFirstPolicy :: SNode s p -> Int
breadthFirstPolicy = SN.depth

-- | Uniform-Cost Search policy implementation
{-# INLINE uniformCostPolicy #-}
uniformCostPolicy :: SNode s p -> p
uniformCostPolicy = SN.cost

-- | Depth-First Search policy implementation
{-# INLINE depthFirstPolicy #-}
depthFirstPolicy :: SNode s p -> Int
depthFirstPolicy n = - (SN.depth n)

-- INFORMED (HEURISTIC) POLICIES --

{-# INLINE greedyBestFirstPolicy #-}
greedyBestFirstPolicy :: (Ord p) => (s -> p) -> SNode s p -> p
greedyBestFirstPolicy h node = h (SN.state node)

{-# INLINE aStarPolicy #-}
aStarPolicy :: (Num p) => (s -> p) -> SNode s p -> p
aStarPolicy h node = h (SN.state node) + SN.cost node
