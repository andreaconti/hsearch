module AI.Games.ZeroSum.Internal
    ( 
    ) where

import qualified Data.AI.Search.SearchRoute.Internal as SR
import           Data.AI.Search.SearchRoute.Internal (SRoute(..))

import qualified Data.AI.Search.SearchNode as SN
import           Data.AI.Search.SearchNode (SNode(..))

import qualified Data.AI.Search.SearchFringe as SF
import           Data.AI.Search.SearchFringe (SFringe)

import           AI.Search.Policies (CheckTime(..), SearchPolicy(..))

-- TYPES --

-- | Player who owns the status in the game node /GNode/
data Player = Min | Max

-- | game tree's node description
data GNode s a = GNode { player ::                Player -- ^ player who will choose the move
                       , state  ::                !s     -- ^ state
                       , depth  :: {-# UNPACK #-} !Int   -- ^ depth
                       , action ::                !a     -- ^ action
                       }


-- FUNCTIONS --

-- QUALI SONO I PARAMETRI?
-- *  se faccio una ricerca in un albero o un grafo
-- \\ la ricerca è depth first
-- *  la profondità massima a cui andare
-- *  utility function
-- *  euristica per i tagli alfa beta
-- *  generatore di nuovi stati, non interessa il costo, ma solo lo stato 
--    e la azione corrispondente che ci porta in quello stato
-- -> restituisce una azione

