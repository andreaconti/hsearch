{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module AI.Search.Examples.EightGame where

import Data.Matrix
import Data.Maybe
import Data.List
import Data.Vector (imap)
import System.Random
import Control.Monad

import GHC.Generics (Generic)
import Control.DeepSeq

import AI.Search
import AI.Search.Policies

--------------------------------------
-- Example of solution of the Eight
-- puzzle game 
--------------------------------------


-- model types
data Card = Empty
          | Card {-# UNPACK #-} !Int
    deriving (Eq, Generic, NFData, Ord)

instance Show Card where
    show (Card x) = "[" ++ show x ++ "]"
    show Empty    = ""

instance Num Card where
    (+) (Card x) (Card y) = Card $ x + y
    (+) Empty    (Card y) = Card y
    (+) (Card x) Empty    = Card x
    (+) Empty    Empty    = Empty

    (*) (Card x) (Card y) = Card $ x * y
    (*) Empty    _        = Empty
    (*) _        Empty    = Empty
    
    abs (Card x) = Card $ abs x
    abs Empty    = Empty
    
    signum (Card x) = Card $ signum x
    signum Empty    = Empty

    fromInteger = Card . fromInteger

    negate (Card x) = Card $ negate x
    negate Empty    = Empty

type Pos = (Int, Int)

data Table = Table { 
                     getTable :: !(Matrix Card)
                   , previous :: !Pos
                   }

instance Eq Table where
    (Table t1 _) == (Table t2 _) = t1 == t2

instance Show Table where
    show (Table t1 _) = show t1

instance Ord Table where
    (Table m1 _) <= (Table m2 _) = sum m1 <= sum m2

-- PROBLEM MODEL --

emptyPos :: Table -> Maybe Pos
emptyPos = listToMaybe . join . toList . mapPos (\(r, c) v -> [(r, c) | v == Empty]) . getTable

findMoves :: Table -> [Pos]
findMoves table@(Table t p) = filter (\x -> bounds x && x /= p) [(ex-1, ey), (ex+1, ey), (ex, ey-1), (ex, ey+1)]
    where (ex, ey) = fromJust $ emptyPos table
          bounds (x, y) = x >= 1 && y >= 1 && x <= nrows t && y <= ncols t

stateGenerator :: Table -> [(Table, Int)]
stateGenerator table = 
                  let ep    = fromJust $ emptyPos table
                      moves = findMoves table
                  in  zip (map (\x -> switchCards ep x table) moves) [1,1..]

switchCards :: Pos -> Pos -> Table -> Table
switchCards (x1, y1) (x2, y2) (Table t _) = 
    let a = getElem x1 y1 t
        b = getElem x2 y2 t
    in  Table (setElem a (x2, y2) . setElem b (x1, y1) $ t) (x1, y1)

goal = (\t -> Table t (-1,-1)) . fromLists $
    [ [Card 1, Card 2, Card 3]
    , [Card 4, Card 5, Card 6]
    , [Card 7, Card 8, Empty ] ]

heuristic (Table t p) = 8 - (foldl' (\acc x -> if x == Card 0 then acc+1 else acc) 0 $ t - getTable goal)

-- UTILS --

genTable :: [[Int]] -> Table
genTable = (\t -> Table t (-1,-1)) . fromLists . map (map (\x -> if x == 0 then Empty else Card x))

randomTable' randoms ops   table | ops <= 0 = table
randomTable' (r:rs)  ops table = let tables  = map fst (stateGenerator table)
                                 in if r >= length tables
                                    then randomTable' rs ops table
                                    else randomTable' rs (ops-1) (tables !! r)

randomTable n = do
    g <- getStdGen
    return $ randomTable' (randomRs (0, 3) g) n goal

-- SOLVER --
solve = search graph (aStarPolicy heuristic) (== goal) stateGenerator
