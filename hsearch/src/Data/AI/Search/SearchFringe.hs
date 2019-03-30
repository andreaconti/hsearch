module Data.AI.Search.SearchFringe
    ( SFringe
    , graph
    , tree

 -- * constructors   
    , empty
    , singleton
    , insert

 -- * getters   
    , next
    ) where

import Data.AI.Search.SearchFringe.Internal
