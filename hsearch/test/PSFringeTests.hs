module PSFringeTests where

import Test.HUnit
import Data.AI.Search.Fringe
import Data.AI.Search.Fringe.PrioritySetFringe
import AI.Search.Policies
import Control.Monad.ST
import Data.STRef
import Data.Maybe
import Control.Monad
import Test.Framework
import Test.Framework.Providers.HUnit

setFringe :: PrioritySetFringe Int String
setFringe = empty


-- in order to test if the fringe removes closed nodes
fillSetFringe = runST $ do
    setFrVar <- newSTRef setFringe
    modifySTRef setFrVar (\f -> insert f (\x -> 0) ["insert1"])
    modifySTRef setFrVar (\f -> insert f (\x -> 0) ["insert2"])
    modifySTRef setFrVar (\f -> insert f (\x -> 0) ["insert3"])
    modifySTRef setFrVar (\f -> fst . fromJust $ next f)
    modifySTRef setFrVar (\f -> insert f (\x -> 0) ["insert3"])
    readSTRef setFrVar

readSetFringe fr = let (fr1, ins1) = fromJust $ next fr
                       (fr2, ins2) = fromJust $ next fr1
                    in  (ins1, ins2)  

pushPopSet = assertEqual "test PrioritySetFringe" ("insert2", "insert1") (readSetFringe fillSetFringe)

testsSetFringe = [ testCase "SetFringe push-pop" pushPopSet ]
