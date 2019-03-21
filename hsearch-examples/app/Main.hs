module Main where

import AI.Search.Examples.NineGame
import Control.Monad

-- test NineTales
main :: IO ()
main = do 
    table <- randomTable 30
    forM_ (solve table) print
