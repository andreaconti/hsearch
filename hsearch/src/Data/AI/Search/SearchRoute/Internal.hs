{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.AI.Search.SearchRoute.Internal
    ( SRoute

    -- * Deconstruction
    , EndPoint(..), endPoint
    , StartPoint(..), startPoint
    
    -- * Construction
    , append
    , singleton
    ) where

import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)


------------------------------------------------
-- Definition
------------------------------------------------

-- | implements a sequence of nodes ideally from root to 
--   leafs of a generic search tree, a route can not be empty
newtype SRoute a = SRoute { getSeq :: Seq a }
    deriving (Ord, Read, Functor, Traversable, Foldable)

instance (Eq a) => Eq (SRoute a) where
    (endPoint -> _ :+ e1) == (endPoint -> _ :+ e2) = e1 == e2

instance (Show a) => Show (SRoute a) where
    show (SRoute seq) = show seq

-------------------------------------------------
-- Construction
-------------------------------------------------

-- | Creates a SearchRoute with an initial element
singleton :: a -> SRoute a
singleton x = SRoute $ Seq.singleton x

-- | Appends an element to an existing SearchRoute
append :: SRoute a -> a -> SRoute a
(SRoute seq) `append` x = SRoute $ seq Seq.|> x

-------------------------------------------------
-- Deconstruction
-------------------------------------------------

-- | Util pattern in order to match end of route
data EndPoint a = SRoute a :+ a
    deriving (Eq, Ord, Show, Read)

data StartPoint a = a :- SRoute a
    deriving (Eq, Ord, Show)

startPoint :: SRoute a -> StartPoint a
startPoint (SRoute (Seq.viewl -> start Seq.:< tail)) = start :- (SRoute tail)

endPoint :: SRoute a -> EndPoint a
endPoint (SRoute (Seq.viewr -> tail Seq.:> end)) = (SRoute tail) :+ end

pattern (:<) :: a -> SRoute a -> SRoute a
pattern v :< route <- (startPoint -> v :- route)

pattern (:>) :: SRoute a -> a -> SRoute a
pattern route :> v <- (endPoint -> route :+ v)
