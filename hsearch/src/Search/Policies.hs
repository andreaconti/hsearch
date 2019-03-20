module Search.Policies 
    ( breadthFirstPolicy
    , uniformCostPolicy
    , depthFirstPolicy
    , greedyBestFirstPolicy
    , aStarPolicy
    , CheckTime(..)
    ) where

import qualified Data.Search.SearchNode as SN
import           Data.Search.SearchNode (SNode(..))
import           Data.Function (on)

-- | To specify if the goal reached check must be done at node generation
-- or at goal expansion
data CheckTime = Generation | Expansion
    deriving Eq

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
greedyBestFirstPolicy h (SNode s _ _) = h s

{-# INLINE aStarPolicy #-}
aStarPolicy :: (s -> Int) -> SNode s -> Int
aStarPolicy h (SNode s _ c) = h s + c

