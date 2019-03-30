{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Int
import Data.List

import AI.Search
import AI.Search.Policies

import Criterion.Main
import Criterion.Types
import Criterion.Main.Options

-- TESTS WITH B=[2,4], D=[5,10,15,20] --

data State = State { depth :: Int, value :: Int }
    deriving Eq


increase :: State -> Int -> State
increase State{..} v = State (depth + 1) (value + 1)   

isValue :: Int -> State -> Bool
isValue v State{..} = value == v

start = State 1 1

-- | generates new states
stateGenerator :: Int -> Int -> State -> [(State, Int)]
stateGenerator branching maxNum state  = let next   = increase state 1
                                             others = take (branching-1) $ map (increase state) [(-1),(-2)..]
                                         in  if depth state >= maxNum
                                                then []
                                                else zip (others ++ [next]) [1,1..]

breadthFirstBench branching depth = search tree breadthFirstPolicy (isValue depth) (stateGenerator branching depth)
depthFirstBench branching depth = search tree depthFirstPolicy (isValue depth) (stateGenerator branching depth) 

-- command line output recap
-- time    : OLS regression, estimates the time needed for a single execution
-- R^2     : goodness-of-fit metric of time, how accurately model fits the observed
--           measurements, it should lie between 0.99 - 1 if measurements are not too
--           noisy
-- mean    : mean execution time
-- std dev : standard deviation

confs = defaultConfig {
          reportFile  = Just "bench-results/report.html"
    }

main = defaultMainWith confs
    [ bgroup "breadth-first-search bench"
        [ bench "B=2, D=5"  $ whnf (breadthFirstBench 2 5)  start
        , bench "B=2, D=10" $ whnf (breadthFirstBench 2 10) start
        , bench "B=2, D=15" $ whnf (breadthFirstBench 2 15) start
        , bench "B=2, D=20" $ whnf (breadthFirstBench 2 20) start
        , bench "B=4, D=5"  $ whnf (breadthFirstBench 4 5)  start
        , bench "B=4, D=10" $ whnf (breadthFirstBench 4 10) start
        {- , bench "B=4, D=15" $ whnf (breadthFirstBench 4 15) start -}
        {- , bench "B=4, D=20" $ whnf (breadthFirstBench 4 20) start -}
        ]
    
   , bgroup "depth-first-search bench"
       [ bench "B=2, D=5"  $ whnf (depthFirstBench 2 5)  start
       , bench "B=2, D=10" $ whnf (depthFirstBench 2 10) start
       , bench "B=2, D=15" $ whnf (depthFirstBench 2 15) start
       , bench "B=2, D=20" $ whnf (depthFirstBench 2 20) start
       , bench "B=4, D=5"  $ whnf (depthFirstBench 4 5)  start
       , bench "B=4, D=10" $ whnf (depthFirstBench 4 10) start
       , bench "B=4, D=15" $ whnf (depthFirstBench 4 15) start
       , bench "B=4, D=20" $ whnf (depthFirstBench 4 20) start
       ]
    
    ]
