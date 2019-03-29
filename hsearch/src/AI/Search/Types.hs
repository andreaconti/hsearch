module AI.Search.Types
    ( MaxDepth(..)
    , CheckTime(..)
    ) where

-----------------------------------------------------------------------------
-- |
-- Module      :  AI.Search.Types
-- Copyright   :  Andrea Conti 2019
-- License     :  BSD-3-Clause
-- Maintainer  :  contiandrea96@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Utility types used in AI.Search modules
--
-----------------------------------------------------------------------------

-- | max depth to search for
data MaxDepth = Forever | Until !Int deriving (Eq, Show)

-- | To check if the target has been reached at Generation time or Expansion time 
data CheckTime = Generation | Expansion
    deriving (Eq, Show)
