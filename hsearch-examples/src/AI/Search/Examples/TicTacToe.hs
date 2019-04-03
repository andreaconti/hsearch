module AI.Search.Examples.TicTacToe where

--------------------------------------
-- Example of modelling tic-tac-toe
-- game
--------------------------------------


import qualified Data.Matrix as M
import           Data.Matrix (Matrix, (!), (<|>))
import qualified Data.Vector as V
import           Data.Vector (Vector)

import           Data.Maybe (catMaybes)

-- Types --

data TicTac = X | O | Empty
    deriving Eq

instance Show TicTac where
    show X = "X"
    show O = "O"
    show Empty = " "

type Table = Matrix TicTac

emptyTable = M.fromList 3 3 (take 9 $ repeat Empty)

-- utils --

getAllTriple t = (map (\x -> M.getRow x $ t <|> M.transpose t) [1..6]) 

allX  triple = (==3) $ V.length . V.filter (==X) $ triple
findX triple = not $ V.null . V.filter (==X) $ triple
findO triple = not $ V.null . V.filter (==O) $ triple
findE triple = not $ V.null . V.filter (==Empty) $ triple


-- Actions and functions --

type Action = Table -> Table

putCross :: Table -> (Int, Int) -> Maybe (Action, Table)
putCross t pos = case t ! pos of
    Empty -> Just $ (M.setElem X pos, M.setElem X pos t)
    _     -> Nothing


generateStates :: Table -> [(Action, Table)]
generateStates t = catMaybes . map (putCross t) $ [(x,y) | x <- [1..3], y <- [1..3] ]

utility :: Table -> Double
utility t = undefined -- TODO
