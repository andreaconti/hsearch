module Search.Examples.NineTales where

import Data.Matrix
import Data.Maybe
import Data.List
import Data.Vector (imap)
import System.Random
import Control.Monad

import Search
import Search.Policies (breadthFirstPolicy, CheckTime(..))


-- model types

type Table = Matrix Card
type Pos = (Int, Int)

data Card = Card Int | Empty
    deriving Eq

instance Show Card where
    show (Card x) = "[" ++ show x ++ "]"
    show Empty    = ""

-- ops and utils

startTable :: RandomGen g => g -> Table
startTable g = let perms = permutations . (++ [Empty]) . map Card $ [1 .. 15]
                   choosen = fst . randomR (0, 100) $ g
               in  fromList 4 4 (perms !! choosen)

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

genTable :: [[Int]] -> Matrix Card
genTable = fromLists . map (map (\x -> if x == 0 then Empty else Card x))

solve = search breadthFirstPolicy Generation (== goal) stateGenerator 
