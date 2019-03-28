module Data.AI.Search.SearchRoute
    ( SRoute(..)

    -- * Deconstruction
    , EndPoint(..), endPoint
    , StartPoint(..), startPoint
    , takeEnd, takeStart
    
    -- * Construction
    , append
    , singleton
    ) where

import Data.AI.Search.SearchRoute.Internal 
