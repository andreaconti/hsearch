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
breadthFirstPolicy :: (Eq s) => SNode s -> Int
breadthFirstPolicy = SN.depth

-- | Uniform-Cost Search policy implementation
uniformCostPolicy :: (Eq s) => SNode s -> Int
uniformCostPolicy n = SN.cost n

-- | Depth-First Search policy implementation
depthFirstPolicy :: (Eq s) => SNode s -> Int
depthFirstPolicy n = - (SN.depth n)

-- INFORMED (HEURISTIC) SEARCH --

greedyBestFirstPolicy :: (s -> Int) -> SNode s -> Int
greedyBestFirstPolicy h (SNode s _ _) = (h s)

aStarPolicy :: (s -> Int) -> SNode s -> Int
aStarPolicy h (SNode s _ c) = (h s) + c

