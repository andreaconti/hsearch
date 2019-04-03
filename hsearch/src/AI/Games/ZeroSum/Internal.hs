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

data Funcs s a = Funcs
                   { utility      :: (s -> Double)
                   , terminalTest :: (GNode s a -> Bool)
                   , generator    :: (s -> [(a, s)])
                   }

{-# INLINE headMay #-}
headMay :: [GNode s a] -> Maybe a
headMay [] = Nothing
headMay (GNode{..}:_) = Just action

-- FUNCTIONS --

{-# INLINE minimax #-}
minimax :: MaxDepth              -- ^ Int: depth until search
        -> (s -> Double)         -- ^ utility function : output must be between [0,1]
        -> (s -> Bool)           -- ^ in order to recognize game terminal states
        -> (s -> [(a, s)])       -- ^ generator of states and actions
        -> s                     -- ^ current state
        -> Maybe a               -- ^ return: best action
minimax m utility terminalTest generator state = do
    guard  $ m > 0
    let terminalTest' = (\(GNode{..}) -> terminalTest state || depth < m)
        fs = Funcs utility terminalTest' generator
    headMay . map fst . sortBy (comparing $ negate . snd) $ map (minValue (-1) 1 fs) (map (fromTuple 0) (generator state))

-- MIN NODES --

{-# INLINE minValue #-}
minValue :: Alpha                 -- ^ best value found for max
         -> Beta                  -- ^ best value found for min
         -> Funcs s a             -- ^ funcs
         -> GNode s a             -- ^ node
         -> (GNode s a, Utility)
minValue alpha beta fs@Funcs{..} node@GNode{..}
    | terminalTest node = (node, utility state)
    | otherwise         = findMin 1 alpha beta fs . map (fromTuple (depth+1)) $ generator state


{-# INLINE findMin #-}
findMin :: Utility -> Alpha -> Beta -> Funcs s a -> [GNode s a] -> (GNode s a, Utility)
findMin v alpha beta fs@Funcs{..} (n:ns) =
    let (node, minV) = maxValue alpha beta fs n
        v' = max v minV
    in case v' <= alpha of
        True -> (node, v')
        False -> findMin v' alpha (min beta v') fs ns

-- MAX NODES --

{-# INLINE maxValue #-}
maxValue :: Alpha                 -- ^ best value found for max
         -> Beta                  -- ^ best value found for min
         -> Funcs s a             -- ^ funzioni
         -> GNode s a             -- ^ node
         -> (GNode s a, Utility)
maxValue alpha beta fs@Funcs{..} node@GNode{..}
    | terminalTest node = (node, utility state)
    | otherwise         = findMax (-1) alpha beta fs . map (fromTuple (depth+1)) $ generator state
    

{-# INLINE findMax #-}
findMax :: Utility -> Alpha -> Beta -> Funcs s a -> [GNode s a] -> (GNode s a, Utility)
findMax v alpha beta fs@Funcs{..} (n:ns) =
    let (node, minV) = minValue alpha beta fs n 
        v' = max v minV
    in case v' >= beta of
        True  -> (node, v')
        False -> findMax v' (max alpha v') beta fs ns
