module AI.Search.Examples.FifteenGame where

import Data.Matrix
import Data.Maybe
import Data.List
import Data.Vector (imap)
import System.Random
import Control.Monad
import Data.Int

import AI.Search
import AI.Search.Policies

-- model types

type Table = Matrix Int8
type Pos = (Int, Int)
type PreviousDirection = Pos

data State = State {-# UNPACK #-} !Pos   -- ^ in order to not go back 
                   {-# UNPACK #-} !Table -- ^ the current puzzle state
    deriving Eq

instance Ord State where
    (State p _) <= (State p1 _) = p <= p1

instance Show State where
    show (State _ t) = show t

-- PROBLEM MODEL --

emptyPos :: Table -> Pos
emptyPos = fromJust . listToMaybe . join . toList . mapPos (\(r, c) v -> [(fromIntegral r, fromIntegral c) | v == 0])

findMoves :: Table -> [Pos]
findMoves t = filter bounds [(ex-1, ey), (ex+1, ey), (ex, ey-1), (ex, ey+1)]
    where (ex, ey) = emptyPos t
          bounds (x, y) = x >= 1 && y >= 1 && x <= nrows t && y <= ncols t

stateGenerator :: State -> [(State, Int)]
stateGenerator (State (x,y) t) = let (ep1, ep2) = emptyPos t
                                     back       = (ep1-x, ep2-y)
                                     moves      = findMoves t \\ [back]
                                 in  zip (map (\x -> State (fst x - fst back, snd x - snd back) (switchCards (ep1, ep2) x t) ) moves) [1,1..]

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

misplaced (State _ t) = 15 - (foldl' (\acc x -> if x == 0 then acc+1 else acc) 0 (t - goal))

{- -- UTILS -- -}

genTable :: [[Int8]] -> Table
genTable = fromLists

randomTable' randoms ops table | ops <= 0    = table
randomTable' (r:rs)  ops table = let tables  = map (\(State _ t, _) -> t) (stateGenerator $ State (-1,-1) table)
                                 in if r >= length tables
                                    then randomTable' rs ops table
                                    else randomTable' rs (ops-1) (tables !! r)

randomTable n = do
    g <- getStdGen
    return $ randomTable' (randomRs (0, 3) g) n goal

{- -- SOLVER -- -}

solve = search graph (aStarPolicy misplaced) (\(State _ t) -> t == goal) stateGenerator



