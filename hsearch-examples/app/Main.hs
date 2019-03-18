module Main where

import Search.Examples.NineTales
import Control.Monad

start = genTable
    [ [2, 0, 3]
    , [5, 1, 6]
    , [4, 7, 8] ]

-- test NineTales
main :: IO ()
main = forM_ (solve start) print
