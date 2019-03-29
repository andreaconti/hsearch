module AI.Search.Examples.Cannibals where

import AI.Search.Types
import AI.Search.Graph.Algorithms
import Control.Monad (guard)
import Data.Maybe

--------------------------------------
-- Example of solution of Cannibals
-- and Missionary :
-- 
--
--------------------------------------

data SpondaBarca = S | D
    deriving (Eq, Ord, Show)

cambiaSponda :: SpondaBarca -> SpondaBarca
cambiaSponda S = D
cambiaSponda D = S

type Stato = (Int, Int, SpondaBarca)

goal :: Stato -> Bool
goal = (==(0,0,D))

applicaOp :: Stato -> (Int, Int) -> Maybe Stato
applicaOp (m, c, sponda) (x, y) = do
    let nc = c + y
        nm = m + x
        valids = [(0,0),(0,1),(0,2),(0,3),(1,1),(2,2),(3,0),(3,1),(3,2),(3,3)]
    {- guard (m + x >= 0 && c + y >= 0) -}
    {- guard (m + x <= 3 && c + y <= 3) -}
    {- guard ( nc == nm || nc - nm  ) -}
    guard ((nm, nc) `elem` valids)
    return $ (m + x, c + y, cambiaSponda sponda)

opsDS = [(1,1),(2,0),(0,2),(1,0),(0,1)]
opsSD = map (\(x,y) -> (-x, -y)) opsDS

stateGenerator :: Stato -> [(Stato, Int)]
stateGenerator stato@(_,_, S) = zip ( catMaybes . map (applicaOp stato) $ opsSD ) [1,1..]
stateGenerator stato@(_,_, D) = zip ( catMaybes . map (applicaOp stato) $ opsDS ) [1,1..]

solve = breadthFirstSearch goal stateGenerator



