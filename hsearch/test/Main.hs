module Main where

import qualified AlgorithmsTests as AT
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit


main :: IO ()
main = defaultMainWithOpts ( AT.testsBreadthFirstSearch
                           )
       mempty
