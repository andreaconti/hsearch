{-# LANGUAGE RecordWildCards #-}

module AI.Games.ZeroSum.Internal
    ( minimax
    , Alpha
    , Beta
    , Utility
    , MaxDepth
    , GNode
    ) where

import qualified Data.AI.Search.SearchRoute.Internal as SR
import           Data.AI.Search.SearchRoute.Internal (SRoute(..))

import qualified Data.AI.Search.SearchFringe as SF
import           Data.AI.Search.SearchFringe (SFringe)

import           AI.Search.Policies (CheckTime(..), SearchPolicy(..))
import           Data.Foldable
import           Control.Monad
import           Data.Function (on)
import           Data.List
import           Data.Ord (comparing)

-- TYPES --

-- | game tree's node description
data GNode s a = GNode { state  ::                !s      -- ^ state
                       , depth  :: {-# UNPACK #-} !Int    -- ^ depth
                       , action ::                a       -- ^ action
                       }

{-# INLINE fromTuple #-}
fromTuple :: Int -> (a, s) -> GNode s a
fromTuple d (a, s) = GNode s d a

type Alpha = Double
type Beta  = Double
type Utility = Double
type MaxDepth = Int

-- UTILS --

data Funcs s a d = Funcs
                   { utility      :: s -> d
                   , terminalTest :: GNode s a -> Bool
                   , generator    :: s -> [(a, s)]
                   }

{-# INLINE headMay #-}
headMay :: [GNode s a] -> Maybe a
headMay [] = Nothing
headMay (GNode{..}:_) = Just action

-- FUNCTIONS --

{-# INLINE minimax #-}
minimax :: (Ord d, Bounded d)
        => MaxDepth              -- ^ Int: depth until search
        -> (s -> d)              -- ^ utility function : high values for Max, low values for Min
        -> (s -> Bool)           -- ^ in order to recognize game terminal states
        -> (s -> [(a, s)])       -- ^ generator of states and actions
        -> s                     -- ^ current state
        -> Maybe a               -- ^ return: best action
minimax m utility terminalTest generator state = do
    guard  $ m > 0
    let terminalTest' GNode{..} = terminalTest state || depth < m
        fs = Funcs utility terminalTest' generator
    headMay . map fst . sortBy (flip compare `on` snd) $ map (minValue minBound maxBound fs . fromTuple 0) (generator state)

-- MIN NODES --

{-# INLINE minValue #-}
minValue :: (Ord d, Bounded d) 
         => d                     -- ^ best value found for max
         -> d                     -- ^ best value found for min
         -> Funcs s a d           -- ^ funcs
         -> GNode s a             -- ^ node
         -> (GNode s a, d)
minValue alpha beta fs@Funcs{..} node@GNode{..}
    | terminalTest node = (node, utility state)
    | otherwise         = findMin maxBound alpha beta fs . map (fromTuple (depth+1)) $ generator state


{-# INLINE findMin #-}
findMin :: (Bounded d, Ord d) => d -> d -> d -> Funcs s a d -> [GNode s a] -> (GNode s a, d)
findMin v alpha beta fs@Funcs{..} (n:ns) =
    let (node, minV) = maxValue alpha beta fs n
        v' = max v minV
    in if v' <= alpha
        then (node, v')
        else findMin v' alpha (min beta v') fs ns

-- MAX NODES --

{-# INLINE maxValue #-}
maxValue :: (Bounded d, Ord d)
         => d                     -- ^ best value found for max
         -> d                     -- ^ best value found for min
         -> Funcs s a d           -- ^ funzioni
         -> GNode s a             -- ^ node
         -> (GNode s a, d)
maxValue alpha beta fs@Funcs{..} node@GNode{..}
    | terminalTest node = (node, utility state)
    | otherwise         = findMax minBound alpha beta fs . map (fromTuple (depth+1)) $ generator state
    

{-# INLINE findMax #-}
findMax :: (Bounded d, Ord d) => d -> d -> d -> Funcs s a d -> [GNode s a] -> (GNode s a, d)
findMax v alpha beta fs@Funcs{..} (n:ns) =
    let (node, minV) = minValue alpha beta fs n 
        v' = max v minV
    in if v' >= beta
        then (node, v')
        else findMax v' (max alpha v') beta fs ns
