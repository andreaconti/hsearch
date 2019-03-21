module PQFringeTests where

import Test.HUnit
import Data.AI.Search.Fringe
import Data.AI.Search.Fringe.PQFringe
import AI.Search.Policies
import Control.Monad.ST
import Data.STRef
import Data.Maybe
import Control.Monad
import Test.Framework
import Test.Framework.Providers.HUnit

fifoFringe :: PQFringe Int String
fifoFringe = empty

fillFifoFringe = runST $ do
    fifoFrVar <- newSTRef fifoFringe
    modifySTRef fifoFrVar (\f -> insert f (\x -> 0) ["insert1"])
    modifySTRef fifoFrVar (\f -> insert f (\x -> 0) ["insert2"])
    modifySTRef fifoFrVar (\f -> insert f (\x -> 0) ["insert3"])
    readSTRef fifoFrVar

readFifoFringe fr = let (fr1, ins1) = fromJust $ next fr
                        (fr2, ins2) = fromJust $ next fr1
                        (fr3, ins3) = fromJust $ next fr2
                    in  (ins1, ins2, ins3)  

testFifoFringe = assertEqual "test PQFringe" ("insert3", "insert2", "insert1") (readFifoFringe fillFifoFringe)

failler = assertEqual "fail!" True False

test :: IO ()
test = defaultMainWithOpts
       [ testCase "PQFringe push-pop" testFifoFringe]
       mempty
