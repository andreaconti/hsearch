module Search.Politics 
    ( breadthFirstPolicy
    ) where

import qualified Data.Search.SearchNode as SN
import           Data.Search.SearchNode (SNode)
import           Data.Function (on)

-- | Breadth-First Search politic implementation, returns the depth
breadthFirstPolicy :: (Eq s) => SNode s -> Int
breadthFirstPolicy = SN.depth
