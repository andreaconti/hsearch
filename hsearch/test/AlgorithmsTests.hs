{-# LANGUAGE RecordWildCards #-}

module AlgorithmsTests where

import Test.HUnit (assertEqual)
import Test.Framework
import Test.Framework.Providers.HUnit
import AI.Search
import AI.Search.Policies
import Data.Int

data State = State { depth :: Int, value :: Int }
    deriving (Eq, Show)

instance Ord State where
    (State _ v1) <= (State _ v2) = v1 <= v2

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
testBreadthFirstSearch1 = assertEqual "check correct error" [] (search graph breadthFirstPolicy (const False) (stateGenerator 2 5) start) 

testsBreadthFirstSearch = [ testCase "breadth-first-search tests" testBreadthFirstSearch1 ]
