module Main where

import qualified PQFringeTests as PQ
import qualified PSFringeTests as PS
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit


main :: IO ()
main = defaultMainWithOpts (  PQ.testsFifoFringe
                           ++ PS.testsSetFringe )
       mempty
