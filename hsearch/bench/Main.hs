module Main where

import Data.Int
import Data.List

import AI.Search.Algorithms
import Criterion.Main
import Criterion.Types
import Criterion.Main.Options

-- TESTS WITH B=[2,4], D=[5,10,15,20] --

type State = Int

-- | generates new states
stateGenerator :: Int -> Int -> State -> [(State, Int8)]
stateGenerator branching maxNum state  = let next   = state + 1
                                             others = take (branching-1) [state-1, state-2..]
                                         in  if state >= maxNum
                                                then []
                                                else zip (others ++ [next]) [1,1..]

breadthFirstBench25  = breadthFirstSearch (== 5)  (stateGenerator 2 5)
breadthFirstBench210 = breadthFirstSearch (== 10) (stateGenerator 2 10)
breadthFirstBench215 = breadthFirstSearch (== 15) (stateGenerator 2 15)
breadthFirstBench220 = breadthFirstSearch (== 20) (stateGenerator 2 20)
breadthFirstBench45  = breadthFirstSearch (== 5)  (stateGenerator 4 5)
breadthFirstBench410 = breadthFirstSearch (== 10) (stateGenerator 4 10)

{- depthFirstBench5  = depthFirstSearch (== 5)  (stateGenerator 2 5) -}
{- depthFirstBench10 = depthFirstSearch (== 10) (stateGenerator 2 10) -}
{- depthFirstBench20 = depthFirstSearch (== 20) (stateGenerator 2 20) -}


-- command line output recap
-- time    : OLS regression, estimates the time needed for a single execution
-- R^2     : goodness-of-fit metric of time, how accurately model fits the observed
--           measurements, it should lie between 0.99 - 1 if measurements are not too
--           noisy
-- mean    : mean execution time
-- std dev : standard deviation

confs = defaultConfig {
        reportFile = Just $ "bench-results/report.html"
    }

main = defaultMainWith confs
    [ bgroup "breadth-first-search bench"
        [ bench "B=2, D=5"  $ whnf breadthFirstBench25  1
        , bench "B=2, D=10" $ whnf breadthFirstBench210 1
        , bench "B=2, D=15" $ whnf breadthFirstBench215 1
        , bench "B=2, D=20" $ whnf breadthFirstBench220 1
        , bench "B=4, D=5"  $ whnf breadthFirstBench45  1
        , bench "B=4, D=10" $ whnf breadthFirstBench410 1
        , bench "B=4, D=15" $ whnf breadthFirstBench415 1
        -- !! Bench B=4 D=15 !! Out of memory (16Gb RAM)
        ]
    
--    , bgroup "depth-first-search bench"
--        [ bench "B=2, D=5"  $ whnf depthFirstBench5  1
--        , bench "B=2, D=10" $ whnf depthFirstBench10 1
--        , bench "B=2, D=20" $ whnf depthFirstBench20 1
--        ]
    
    ]
