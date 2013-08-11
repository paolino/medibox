{-# LANGUAGE TemplateHaskell #-}


-- | Sequenza's forms a graph of dependencies. Bases are roots, indipendent nodes. Highers are dependent node, each of their patterns point to one node Higher or Base, so it's a multidependent graph. It must be acyclic. The pattern index of bases point to an stone set pattern formed by a regular pattern of index value equispaced points.


module SequenzaGraph where


import qualified Data.IntMap as IM
import Data.IntMap (IntMap)

import Control.Lens

import Sequenza
import Graph


data SG = SG 
        { _sgMap :: IntMap (Sequenza, Maybe [(Tempo,Int)])
        , _sgDeps :: Graph Int
        }
        deriving (Show,Read)

$(makeLenses ''SG)





