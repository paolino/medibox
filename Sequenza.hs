{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction, TypeSynonymInstances, FlexibleInstances #-} -- , NoMonomorphismRestriction, ViewPatterns, FlexibleInstances, Rank2Types #-}       

module Sequenza where

import Data.List -- (groupBy, partition, sort)
import Data.Ord
import Data.Function (on)
import qualified Data.IntMap as M
import Data.IntMap (IntMap)

import Control.Arrow ((&&&),first,second, (***))
--------------------------------
import Control.Lens 
import Control.Lens.Tuple

import Language.Haskell.TH.Lens

-------------------------------
import Haskell
import Dynamic


from128 x = 1/128 * fromIntegral x
from128p x = from128 x - 0.5

type Tempo = Double

data Pattern = Pattern 
        { _pnumber :: Int
        , _pwidth :: Int
        , _pshift :: Int
        }
        deriving (Show,Read)

$(makeLenses ''Pattern)

instance Present Pattern where
        zero = Pattern 0 1 0

-- | A Sequenza is a sum of patterns Int indexed. The Base/Higher tags select if the Sequenza is made of pure patterns or its 'pnumber' are referring to other Sequenzas
type Sequenza = IntMap Pattern  

-- | A Score is the result of a Sequenza computation
type Score = [(Tempo,Int)]

-- | A set of interdipendent Sequenza each coupled with its result, for dynamic programming
type DSequenze = Dynamic Sequenza Sequenza Score 

scoreOfBase :: Sequenza -> Score
scoreOfBase seq = flip zip (repeat 1) . sort . concat $ every (M.elems seq) baseEvents where
        baseEvents (Pattern n w s) = every [from128 s + 1 / fromIntegral w / fromIntegral n * fromIntegral j | j <- [0 .. n - 1]] (`floatMod` 1)

scoreOfPointing :: [Score] -> Sequenza -> Score
scoreOfPointing tss seq = sort . concat $ zipWith pointingEvents (M.elems seq) tss where
        pointingEvents (Pattern _ w s) = map $ first (\x -> (from128 s + x / fromIntegral w) `floatMod` 1)

seqDeps :: Sequenza -> [Int]
seqDeps = map _pnumber . M.elems

querySequenza :: DSequenze -> Int -> Maybe (Pointing Sequenza Sequenza,Score,DSequenze)
querySequenza = query seqDeps scoreOfPointing scoreOfBase 

insertSequenza ::  Int -> Pointing Sequenza Sequenza -> DSequenze -> Maybe DSequenze
insertSequenza = dyninsert seqDeps
 

unsafeMapLens :: Int -> Lens Sequenza Sequenza Pattern Pattern
unsafeMapLens n = lens (M.! n) (flip $ M.insert n) 


sequenza_lenses = 
        [  unsafeMapLens 0 . pnumber
        ,  unsafeMapLens 1 . pnumber
        ,  unsafeMapLens 2 . pnumber
        ,  unsafeMapLens 3 . pnumber
        ,  unsafeMapLens 4 . pnumber
        ,  unsafeMapLens 5 . pnumber
        ,  unsafeMapLens 6 . pnumber
        ,  unsafeMapLens 7 . pnumber
        ,  unsafeMapLens 0 . pwidth
        ,  unsafeMapLens 1 . pwidth
        ,  unsafeMapLens 2 . pwidth
        ,  unsafeMapLens 3 . pwidth
        ,  unsafeMapLens 4 . pwidth
        ,  unsafeMapLens 5 . pwidth
        ,  unsafeMapLens 6 . pwidth
        ,  unsafeMapLens 7 . pwidth
        ,  unsafeMapLens 0 . pshift
        ,  unsafeMapLens 1 . pshift
        ,  unsafeMapLens 2 . pshift
        ,  unsafeMapLens 3 . pshift
        ,  unsafeMapLens 4 . pshift
        ,  unsafeMapLens 5 . pshift
        ,  unsafeMapLens 6 . pshift
        ,  unsafeMapLens 7 . pshift
        ] 


nolens :: Functor f => (Int -> f Int) -> a -> f a
nolens = lens (const 0) (\s _ -> s)


instance Present Sequenza where
        zero = M.fromList $ zip [0..7] $ repeat zero


