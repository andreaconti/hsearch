module Data.Search.Impl.RootSearchNode 
    ( RSNode(..)
    , rsNodeToList
    ) where

import Data.Search.SearchNode

data RSNode s = Root 
              | RSNode
                { root :: RSNode s
                , node :: s
                } deriving Eq

rsNodeToList' :: [s] -> RSNode s -> [s]
rsNodeToList' ls Root = ls
rsNodeToList' ls (RSNode root node) = rsNodeToList' (node : ls) root 

rsNodeToList :: RSNode s -> [s]
rsNodeToList = rsNodeToList' []

