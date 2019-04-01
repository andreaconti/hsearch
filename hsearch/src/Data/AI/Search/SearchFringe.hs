module Data.AI.Search.SearchFringe
    ( SFringe
    , tree
    , graph

 -- * constructors   
    , emptyTree
    , emptyGraph
    , empty
    , singleton
    , insert

 -- * getters   
    , next
    ) where

import Data.AI.Search.SearchFringe.Internal
