module Main where

import AI.Search.Examples.FifteenGame
import Control.Monad

start = genTable [ [13,5,3,15]
                 , [7,2,1,11]
                 , [6,9,8,4]
                 , [10,14,0,12]]

-- test NineTales
main :: IO ()
main = do 
    {- table <- randomTable 20 -}
    putStrLn "SOLVING:"
    print start
    putStrLn "======"
    forM_ (solve (State (-1,-1) start)) print
