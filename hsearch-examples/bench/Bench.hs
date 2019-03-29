module Main where

import AI.Search.Examples.EightPuzzle
import Criterion.Main
import Criterion.Types
import Control.Monad

moves2 = genTable [ [1,2,3]
                  , [4,5,6]
                  , [0,7,8] ]

moves4 = genTable [ [0,2,3]
                  , [1,5,6]
                  , [4,7,8] ]

moves8 = genTable [ [2,5,3]
                  , [1,7,6]
                  , [4,0,8] ]

generic = genTable [ [8,0,3]
                   , [6,5,4]
                   , [7,1,2] ]

main = defaultMain [
    bgroup "eight puzzle" [
        bench "2 moves" $ whnf solve moves2 ,
        bench "4 moves" $ whnf solve moves4 ,
        bench "8 moves" $ whnf solve moves8 ]
    ]
