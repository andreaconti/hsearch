{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Int
import Data.List

import AI.Search.Algorithms
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
stateGenerator :: Int -> Int -> State -> [(State, Int8)]
stateGenerator branching maxNum state  = let next   = increase state 1
                                             others = take (branching-1) $ map (increase state) [(-1),(-2)..]
                                         in  if depth state >= maxNum
                                                then []
                                                else zip (others ++ [next]) [1,1..]

breadthFirstBench25  = breadthFirstSearch (isValue 5)  (stateGenerator 2 5)
breadthFirstBench210 = breadthFirstSearch (isValue 10) (stateGenerator 2 10)
breadthFirstBench215 = breadthFirstSearch (isValue 15) (stateGenerator 2 15)
breadthFirstBench220 = breadthFirstSearch (isValue 20) (stateGenerator 2 20)
breadthFirstBench45  = breadthFirstSearch (isValue 5)  (stateGenerator 4 5)
breadthFirstBench410 = breadthFirstSearch (isValue 10) (stateGenerator 4 10)

depthFirstBench5  = depthFirstSearch (isValue 5)  (stateGenerator 2 5)
depthFirstBench10 = depthFirstSearch (isValue 10) (stateGenerator 2 10)
depthFirstBench20 = depthFirstSearch (isValue 20) (stateGenerator 2 20)


-- command line output recap
-- time    : OLS regression, estimates the time needed for a single execution
-- R^2     : goodness-of-fit metric of time, how accurately model fits the observed
--           measurements, it should lie between 0.99 - 1 if measurements are not too
--           noisy
-- mean    : mean execution time
-- std dev : standard deviation

confs = defaultConfig {
          reportFile  = Just $ "bench-results/report.html"
        , regressions = [(["allocated", "numGcs"], "iters")]
    }

main = defaultMainWith confs
    [ bgroup "breadth-first-search bench"
        [ bench "B=2, D=5"  $ whnf breadthFirstBench25  start
--        , bench "B=2, D=10" $ whnf breadthFirstBench210 start
--        , bench "B=2, D=15" $ whnf breadthFirstBench215 start
--        , bench "B=2, D=20" $ whnf breadthFirstBench220 start
--        , bench "B=4, D=5"  $ whnf breadthFirstBench45  start
--        , bench "B=4, D=10" $ whnf breadthFirstBench410 start
        -- !! Bench B=4 D=15 !! Out of memory (16Gb RAM)
        ]
    
   , bgroup "depth-first-search bench"
       [ bench "B=2, D=5"  $ whnf depthFirstBench5  start
--        , bench "B=2, D=10" $ whnf depthFirstBench10 start
--        , bench "B=2, D=20" $ whnf depthFirstBench20 start
       ]
    
    ]
