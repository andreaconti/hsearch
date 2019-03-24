{-# LANGUAGE RecordWildCards #-}

module AlgorithmsTests where

import Test.HUnit (assertEqual)
import Test.Framework
import Test.Framework.Providers.HUnit
import AI.Search.Algorithms
import Data.Int

data State = State { depth :: Int, value :: Int }
    deriving (Eq, Show)

increase :: State -> Int -> State
increase State{..} v = State (depth + 1) (value + 1)   

isValue :: Int -> State -> Bool
isValue v State{..} = value == v

start = State 1 1

-- | generates new states
stateGenerator :: Int -> Int -> State -> [(State, Int8)]
stateGenerator branching maxNum state  = let next   = increase state 1
                                             others = take (branching-1) $ map (increase state) [(-1),(-2)..]
                                         in  if depth state >= maxNum
                                                then []
                                                else zip (others ++ [next]) [1,1..]

-- check end not found
testBreadthFirstSearch1 = assertEqual "check correct error" [] (breadthFirstSearch (const False) (stateGenerator 2 5) start) 

testsBreadthFirstSearch = [ testCase "correct result not found" testBreadthFirstSearch1 ]
