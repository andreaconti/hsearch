module AI.Search.Examples.TicTacToe where

--------------------------------------
-- Example of modelling tic-tac-toe
-- game
--------------------------------------


import qualified Data.Matrix as M
import           Data.Matrix (Matrix, (!), (<|>))
import qualified Data.Vector as V
import           Data.Vector (Vector)

import           Data.Maybe (catMaybes, mapMaybe, fromJust)

import           AI.Games.ZeroSum (minimax)

-- Types --

data TicTac = X | O | Empty
    deriving Eq

instance Show TicTac where
    show X = "X"
    show O = "O"
    show Empty = " "

type Table = Matrix TicTac

emptyTable = M.fromList 3 3 (replicate 9 Empty)

-- utils --

getAllTriple t = V.map (\x -> M.getCol x $ t <|> M.transpose t) (V.iterateN 6 (+1) 1)

allX  triple = (==3) $ V.length . V.filter (==X) $ triple
findX triple = not $ V.null . V.filter (==X) $ triple
allO  triple = (==3) $ V.length . V.filter (==O) $ triple
findO triple = not $ V.null . V.filter (==O) $ triple
findE triple = not $ V.null . V.filter (==Empty) $ triple
onlyXE triple = findX triple && findE triple && not (findO triple)
onlyOE triple = findO triple && findE triple && not (findX triple)

-- Actions and functions --

type Action = Table -> Table

putSign :: Table -> TicTac -> (Int, Int) -> Maybe (Action, Table)
putSign t symbol pos = case t ! pos of
    Empty -> Just (M.setElem symbol pos, M.setElem symbol pos t)
    _     -> Nothing


generateStates :: Table -> [(Action, Table)]
generateStates t = mapMaybe (putSign t X) [(x,y) | x <- [1..3], y <- [1..3] ]

utility :: Table -> Int
utility t = let triples = getAllTriple t
                good    = V.length . V.filter onlyXE $ triples
                bad     = V.length . V.filter onlyOE $ triples
            in  good - bad

endGame :: Table -> Bool
endGame t = any (\x -> allX x || allO x) (getAllTriple t) 

nextCross = minimax 30 utility endGame generateStates

-- terminal game --

meAndYou :: Table -> IO Table
meAndYou t = do
    let f  = fromJust . nextCross $ t
        t' = f t

    print t'
    putStr "Enter action: "
    pos <- read <$> getLine :: IO (Int, Int) 
    let t'' = snd . fromJust . putSign t' O $ pos 
    if endGame t' || endGame t''
        then do { print t'' ; putStrLn "END GAME"; return t'' }
        else meAndYou t''

game :: IO ()
game = do 
    meAndYou emptyTable
    return ()
