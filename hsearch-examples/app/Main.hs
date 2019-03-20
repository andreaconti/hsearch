module Main where

import Search.Examples.NineTales
import Control.Monad

start = genTable
    [ [2,3,6]
    , [1,7,5]
    , [4,0,8] ]

-- test NineTales
main :: IO ()
main = forM_ (solve start) print
