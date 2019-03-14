module Examples.NineTales where

{- import Search -}
{- import Search.Politics (breadthFirstPolicy) -}
{- import Data.Search.Frontier.PQFrontier -}
{- import Data.Search.SearchNode -}

{- import Data.List -}
{- import Control.Monad -}
{- import Data.Maybe -}

{- -- | Usage Example with the 9-tales game -}

{- type Table a = [[a]] -}
{- newtype Card = Card { getValue :: Int } -}
    {- deriving Eq -}

{- instance Show Card where -}
    {- show (Card i) = show i -}

{- goal = map (map Card)  -}
       {- [ [1,2,3] -}
       {- , [4,5,6] -}
       {- , [7,8,0] ] -}

{- generateOps :: Int -> [Int -> Int] -}
{- generateOps index = let ops = [(+1), (\x -> x - 1), (+3), (\x -> x - 3)] -}
                    {- in  filter (\op -> op index < 9 && op index >= 0) ops -}

{- swapElementsAt :: Int -> Int -> [a] -> [a] -}
{- swapElementsAt i j xs = let elemI = xs !! i -}
                            {- elemJ = xs !! j -}
                            {- left = take i xs -}
                            {- middle = take (j - i - 1) (drop (i + 1) xs) -}
                            {- right = drop (j + 1) xs -}
                    {- in  left ++ [elemJ] ++ middle ++ [elemI] ++ right -}

{- toTable :: [Card] -> [[Card]] -}
{- toTable [] = [] -}
{- toTable ls = let (l, t) = splitAt 3 ls in l : toTable t -}

{- generateStates :: Table Card -> [(Table Card, Int)] -}
{- generateStates t = let flatt = join t -}
                       {- zeroIndex = fromJust . elemIndex (Card 0) $ flatt -}
                       {- swappes = (generateOps zeroIndex) <*> [zeroIndex] -}
                   {- in  zip (map (\i -> toTable . swapElementsAt zeroIndex i $ flatt) $ swappes) [1..] -}

{- config :: SearchConfig (PQFrontier Int (SNode (Table Card))) (Table Card) -}
{- config = SearchConfig -}
    {- { generator = generateStates -}
    {- , checkGoal = (== goal) -}
    {- , frontier  = priorityQueueFrontier breadthFirstPolicy -}
    {- , checkTime = Expansion -}
    {- } -}

{- nineTalesSolver = search config -}
