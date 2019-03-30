module AI.Search.Examples.Cannibals where

import AI.Search
import AI.Search.Policies
import Control.Monad (guard)
import Data.Maybe

--------------------------------------
-- Example of solution of Cannibals
-- and Missionary :
-- Help the 3 cannibals and 3 missionaries to move to the other side of the lake. 
-- Note that when there are more cannibals on one side of the lake than 
-- missionaries, the cannibals eat them. There is a ship which can carry a
-- maximum of 2 people. The goal is to have 3 cannibals and 3 missionaries on the
-- right side of the lake.
--------------------------------------

data ShipSide = L | R
    deriving (Eq, Ord, Show)

switchSide :: ShipSide -> ShipSide
switchSide L = R
switchSide R = L

type LeftSide = (Int, Int, ShipSide)

goal :: LeftSide -> Bool
goal = (==(0,0,R))

applicaOp :: LeftSide -> (Int, Int) -> Maybe LeftSide
applicaOp (m, c, side) (x, y) = do
    let nc = c + y
        nm = m + x
        valids = [(0,0),(0,1),(0,2),(0,3),(1,1),(2,2),(3,0),(3,1),(3,2),(3,3)]
    guard ((nm, nc) `elem` valids)
    return (m + x, c + y, switchSide side)

opsRL = [(1,1),(2,0),(0,2),(1,0),(0,1)]
opsLR = map (\(x,y) -> (-x, -y)) opsRL

stateGenerator :: LeftSide -> [(LeftSide, Int)]
stateGenerator stato@(_,_, L) = zip ( mapMaybe (applicaOp stato) opsLR ) [1,1..]
stateGenerator stato@(_,_, R) = zip ( mapMaybe (applicaOp stato) opsRL ) [1,1..]

solve = search graph breadthFirstPolicy goal stateGenerator
