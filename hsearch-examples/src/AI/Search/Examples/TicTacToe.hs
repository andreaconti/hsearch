{-# LANGUAGE MultiWayIf #-}
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

import           System.Console.Haskeline

-- Types --

data TicTac = X | O | E
    deriving Eq

instance Show TicTac where
    show X = "X"
    show O = "O"
    show E = " "

type Table = Matrix TicTac

emptyTable = M.fromList 3 3 (replicate 9 E)

-- utils --

getAllSequences t = V.map (\x -> M.getCol x $ t <|> M.transpose t) (V.iterateN 6 (+1) 1) `V.snoc` M.getDiag t `V.snoc` V.fromList [t ! (1,3), t ! (2,2), t ! (3,1)]

numIn :: Int -> Int -> Int -> Table -> Int
numIn numX numO numE t = V.length . V.filter (found numX numO numE) $ getAllSequences t
    where found x o e v = let x' = V.length . V.filter (==X) $ v
                              o' = V.length . V.filter (==O) $ v
                              e' = V.length . V.filter (==E) $ v
                          in  x == x' && o == o' && e == e'

-- Actions and functions --

type Action = Table -> Table

putSign :: Table -> TicTac -> (Int, Int) -> Maybe (Action, Table)
putSign t symbol pos = case t ! pos of
    E -> Just (M.setElem symbol pos, M.setElem symbol pos t)
    _ -> Nothing


generateStates :: Table -> [(Action, Table)]
generateStates t = mapMaybe (putSign t X) [(x,y) | x <- [1..3], y <- [1..3] ]

utility :: Table -> Int
utility t = let allx = numIn 3 0 0 t
                allo = numIn 0 3 0 t
                x2e1 = numIn 2 0 1 t
                o2e1 = numIn 0 2 1 t
                x1e2 = numIn 1 0 2 t
                o1e2 = numIn 0 1 2 t
            in if | emptyTable == t -> 0 
                  | allx >= 1       -> 300
                  | allo >= 1       -> (-300)
                  | otherwise       -> x2e1 * 50 - o2e1 * 100 + x1e2 * 20 - o1e2 * 20

endGame :: Table -> Bool
endGame t = numIn 3 0 0 t >= 1 || numIn 0 3 0 t >= 1 || notElem E (M.toList t)

nextCross = minimax 30 utility endGame generateStates

-- terminal game --

actionAI :: Table -> InputT IO Table
actionAI t = do
    outputStrLn $ show t
    let f  = fromJust . nextCross $ t
        t' = f t
    return t'

actionPlayer :: Table -> InputT IO Table
actionPlayer t = do
    outputStrLn $ show t
    pos <- read . fromJust <$> getInputLineWithInitial "Enter (Int, Int) >> " ("", "") :: InputT IO (Int, Int)
    let t' = snd . fromJust . putSign t O $ pos
    return t'

game' :: Table -> [Table -> InputT IO Table] -> InputT IO ()
game' t (p:ps) = do 
    t' <- p t
    if endGame t'
        then outputStrLn "END GAME"
        else game' t' (ps ++ [p])

game :: IO ()
game = runInputT defaultSettings $ game' emptyTable [actionPlayer, actionAI]

