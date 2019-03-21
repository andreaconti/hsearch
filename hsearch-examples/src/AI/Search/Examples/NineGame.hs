module AI.Search.Examples.NineGame where

import Data.Matrix
import Data.Maybe
import Data.List
import Data.Vector (imap)
import System.Random
import Control.Monad

import AI.Search.Algorithms

-- model types

type Table = Matrix Card
type Pos = (Int, Int)

data Card = Empty
          | Card {-# UNPACK #-} !Int
    deriving Eq

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

-- PROBLEM MODEL --

emptyPos :: Table -> Maybe Pos
emptyPos = listToMaybe . join . toList . mapPos (\(r, c) v -> [(r, c) | v == Empty])

findMoves :: Table -> [Pos]
findMoves t = filter bounds [(ex-1, ey), (ex+1, ey), (ex, ey-1), (ex, ey+1)]
    where (ex, ey) = fromJust $ emptyPos t
          bounds (x, y) = x >= 1 && y >= 1 && x <= nrows t && y <= ncols t

stateGenerator :: Table -> [(Table, Int)]
stateGenerator t = let ep    = fromJust $ emptyPos t
                       moves = findMoves t
                   in  zip (map (\x -> switchCards ep x t) moves) [1..]

switchCards :: Pos -> Pos -> Table -> Table
switchCards (x1, y1) (x2, y2) t = 
    let a = getElem x1 y1 t
        b = getElem x2 y2 t
    in  setElem a (x2, y2) . setElem b (x1, y1) $ t

goal = fromLists
    [ [Card 1, Card 2, Card 3]
    , [Card 4, Card 5, Card 6]
    , [Card 7, Card 8, Empty ] ]

heuristic t = 8 - (foldl' (\acc x -> if x == Card 0 then acc+1 else acc) 0 $ t - goal)

-- UTILS --

genTable :: [[Int]] -> Matrix Card
genTable = fromLists . map (map (\x -> if x == 0 then Empty else Card x))

randomTable' randoms ops   table | ops <= 0 = table
randomTable' (r:rs)  ops table = let tables  = map fst (stateGenerator table)
                                 in  randomTable' rs (ops-1) (tables !! r)

randomTable n = do
    g <- getStdGen
    return $ randomTable' (randomRs (0, 1) g) n goal

-- SOLVER --

solve = iterativeAStarSearch heuristic (== goal) stateGenerator 
