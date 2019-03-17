module Search.Politics 
    ( breadthFirstPolicy
    , CheckTime(..)
    ) where

import qualified Data.Search.SearchNode as SN
import           Data.Search.SearchNode (SNode)
import           Data.Function (on)

-- | To specify if the goal reached check must be done at node generation
-- or at goal expansion
data CheckTime = Generation | Expansion
    deriving Eq

-- | Breadth-First Search politic implementation, returns the depth
breadthFirstPolicy :: (Eq s) => SNode s -> Int
breadthFirstPolicy = SN.depth

-- | Depth-First Search Policy politic implementation
depthFirstPolicy :: (Eq s) => SNode s -> Int
depthFirstPolicy n = undefined
