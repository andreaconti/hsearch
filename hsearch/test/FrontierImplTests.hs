module FrontierImplTests where

import Test.HUnit
import Data.AI.Search.Frontier
import Data.AI.Search.Frontier.PQFrontier
import Search.Policies
import Data.AI.Search.SearchNode
import Control.Monad.ST
import Data.STRef
import Data.Maybe
import Control.Monad
import Test.Framework
import Test.Framework.Providers.HUnit

fifoFrontier :: PQFrontier Int (SNode String)
fifoFrontier = priorityQueueFrontier depth

fillFifoFrontier = runST $ do
    fifoFrVar <- newSTRef fifoFrontier
    modifySTRef fifoFrVar (flip insert $ [SNode "insert1" 1 0])
    modifySTRef fifoFrVar (flip insert $ [SNode "insert2" 2 0])
    modifySTRef fifoFrVar (flip insert $ [SNode "insert3" 3 0])
    readSTRef fifoFrVar

readFifoFrontier fr = let (fr1, ins1) = fromJust $ next fr
                          (fr2, ins2) = fromJust $ next fr1
                          (fr3, ins3) = fromJust $ next fr2
                      in  (state ins1, state ins2, state ins3)

testFifoFrontier = assertEqual "test PQFrontier" ("insert1", "insert2", "insert3") (readFifoFrontier fillFifoFrontier)

failler = assertEqual "fail!" True False

main :: IO ()
main = defaultMainWithOpts
       [ testCase "push-pop" testFifoFrontier]
       mempty
