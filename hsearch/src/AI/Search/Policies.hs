module AI.Search.Policies 
    ( 
   -- * not informed policies
      breadthFirstPolicy
    , uniformCostPolicy
    , depthFirstPolicy

   -- * informed (heuristic) policies  
    , greedyBestFirstPolicy
    , aStarPolicy
    ) where

import           Data.Function (on)
import qualified Data.AI.Search.SearchNode as SN
import           Data.AI.Search.SearchNode (SNode)

-----------------------------------------------------------------------------
-- |
-- Module      :  AI.Search.Policies
-- Copyright   :  Andrea Conti 2019
-- License     :  BSD-3-Clause
-- Maintainer  :  contiandrea96@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- generic policies which can be applied to algorithms
-- in `AI.Search.[Tree|Graph].Generics`
--
-----------------------------------------------------------------------------


-----------------------------------
-- not informed policies
-----------------------------------

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

-----------------------------------
-- informed (heuristic) policies
-----------------------------------

{-# INLINE greedyBestFirstPolicy #-}
greedyBestFirstPolicy :: (Ord p) => (s -> p) -> SNode s p -> p
greedyBestFirstPolicy h node = h (SN.state node)

{-# INLINE aStarPolicy #-}
aStarPolicy :: (Num p) => (s -> p) -> SNode s p -> p
aStarPolicy h node = h (SN.state node) + SN.cost node
