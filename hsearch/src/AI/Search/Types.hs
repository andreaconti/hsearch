module AI.Search.Types
    ( MaxDepth(..)
    , CheckTime(..)
    ) where

-- | max depth to search for
data MaxDepth = Forever | Until !Int deriving (Eq, Show)

-- | To check if the target has been reached at Generation time or Expansion time 
data CheckTime = Generation | Expansion
    deriving (Eq, Show)
