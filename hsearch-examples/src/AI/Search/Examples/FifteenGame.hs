module AI.Search.Examples.FifteenGame where

import Data.Matrix
import Data.Maybe
import Data.List
import Data.Vector (imap)
import System.Random
import Control.Monad
import Data.Int

import AI.Search.Algorithms

-- model types

type Table = Matrix Int8
type Pos = (Int, Int)

-- PROBLEM MODEL --

emptyPos :: Table -> Maybe Pos
emptyPos = listToMaybe . join . toList . mapPos (\(r, c) v -> [(fromIntegral r, fromIntegral c) | v == 0])

findMoves :: Table -> [Pos]
findMoves t = filter bounds [(ex-1, ey), (ex+1, ey), (ex, ey-1), (ex, ey+1)]
    where (ex, ey) = fromJust $ emptyPos t
          bounds (x, y) = x >= 1 && y >= 1 && x <= nrows t && y <= ncols t

stateGenerator :: Table -> [(Table, Int8)]
stateGenerator t = let ep    = fromJust $ emptyPos t
                       moves = findMoves t
                   in  zip (map (\x -> switchCards ep x t) moves) [1,1..]

switchCards :: Pos -> Pos -> Table -> Table
switchCards (x1, y1) (x2, y2) t = 
    let a = getElem x1 y1 t
        b = getElem x2 y2 t
    in  setElem a (x2, y2) . setElem b (x1, y1) $ t

goal :: Table
goal = fromLists
    [ [1, 2,  3, 4 ]
    , [5, 6,  7, 8 ]
    , [9, 10, 11, 12]
    , [13, 14, 15, 0] ]

misplaced t = 15 - (foldl' (\acc x -> if x == 0 then acc+1 else acc) 0 $ t - goal)

{- {- positions = toList . mapPos (\p _ -> p) . fromLists $ map (map Card) [[0,1,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15]] -} -}

{- {- manhattan t = sum . mapPos (\(cr, cc) x -> let (r, c) = positions !! (fromCard x) in (abs cr - r) + (abs cc - c)) $ t -} -}

{- -- UTILS -- -}

genTable :: [[Int8]] -> Table
genTable = fromLists

randomTable' randoms ops   table | ops <= 0 = table
randomTable' (r:rs)  ops table = let tables  = map fst (stateGenerator table)
                                 in if r >= length tables
                                    then randomTable' rs ops table
                                    else randomTable' rs (ops-1) (tables !! r)

randomTable n = do
    g <- getStdGen
    return $ randomTable' (randomRs (0, 3) g) n goal

{- -- SOLVER -- -}

solve = aStarSearch misplaced (== goal) stateGenerator 
