module Main where

import Search.Examples.NineTales
import Control.Monad

-- test NineTales
main :: IO ()
main = do 
    table <- randomTable 30
    forM_ (solve table) print
