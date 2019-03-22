module Main where

import AI.Search.Examples.NineGame
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
                  
config = defaultConfig {
    resamples = 1
}

main = defaultMainWith config [
    bgroup "nine-game" [
        bench "2 moves" $ nf solve moves2 ,
        bench "4 moves" $ nf solve moves4 ,
        bench "8 moves" $ nf solve moves8 ]
    ]
