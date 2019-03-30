{-# LANGUAGE RankNTypes #-}
module AI.Search.Policies 
    ( 
   -- * types
      CheckTime(..)
    , SearchPolicy(..)

   -- * not informed policies
    , breadthFirstPolicy
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
-- types
-----------------------------------

-- | To check if the target has been reached at Generation time or Expansion time 
data CheckTime = Generation | Expansion
    deriving (Eq, Show)

data SearchPolicy s p i = SearchPolicy { policy    :: SNode s p -> i
                                       , checkTime :: CheckTime
                                       }

-----------------------------------
-- not informed policies
-----------------------------------

-- | Breadth-First Search policy implementation, returns the depth
{-# INLINE breadthFirstPolicy #-}
breadthFirstPolicy :: SearchPolicy s p Int
breadthFirstPolicy = SearchPolicy (\n -> SN.depth n) Generation

-- | Uniform-Cost Search policy implementation
{-# INLINE uniformCostPolicy #-}
uniformCostPolicy :: (Ord p) => SearchPolicy s p p
uniformCostPolicy = SearchPolicy SN.cost Expansion

-- | Depth-First Search policy implementation
{-# INLINE depthFirstPolicy #-}
depthFirstPolicy :: SearchPolicy s p Int
depthFirstPolicy = SearchPolicy (\n -> - (SN.depth n)) Generation

-----------------------------------
-- informed (heuristic) policies
-----------------------------------

{-# INLINE greedyBestFirstPolicy #-}
greedyBestFirstPolicy :: (Ord i) => (s -> i) -> SearchPolicy s p i
greedyBestFirstPolicy h = SearchPolicy (\node -> h $ SN.state node) Generation

{-# INLINE aStarPolicy #-}
aStarPolicy :: (Num p) => (s -> p) -> SearchPolicy s p p
aStarPolicy h = SearchPolicy (\node -> h (SN.state node) + SN.cost node) Generation
