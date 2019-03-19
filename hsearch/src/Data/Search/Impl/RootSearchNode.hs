module Data.Search.Impl.RootSearchNode 
    ( RSNode(..)
    , rsNodeToList
    ) where

import Data.Search.SearchNode
import Control.Lens

data RSNode s = Root 
              | RSNode !(RSNode s) !s
    deriving (Show,Eq)

root :: RSNode s -> RSNode s
root Root         = Root
root (RSNode r _) = r

node :: RSNode s -> s
node (RSNode _ n) = n

-- INSTANCES --

instance Functor RSNode where
    fmap f Root = Root
    fmap f (RSNode root node) = RSNode (fmap f root) (f node)

-- TOOLS --

rsNodeToList' :: [s] -> RSNode s -> [s]
rsNodeToList' ls Root = ls
rsNodeToList' ls (RSNode root node) = rsNodeToList' (node : ls) root 

rsNodeToList :: RSNode s -> [s]
rsNodeToList = rsNodeToList' []

