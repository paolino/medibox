{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, ViewPatterns, FlexibleInstances, Rank2Types #-}       

module Sequenza where

import Data.List (groupBy, partition, sort)
import Data.Ord
import Data.Function (on)
import qualified Data.IntMap as M
import Data.IntMap (IntMap)

import Control.Arrow ((&&&),first)
--------------------------------
import Control.Lens 
import Control.Lens.Tuple

import Language.Haskell.TH.Lens

-------------------------------
import Haskell

correct' xs@(x:_) ys = let 
        (ms,us) = partition ((== x) . fst)  ys
        in case ms of 
                [] -> True
                [(_,y)] ->  not (y `elem` xs) && correct' (y:xs) us
                _ -> False

correct x ys = correct' [x] ys


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
       
data Presence = Base (IntMap Pattern) | Higher (IntMap Pattern) deriving (Show,Read)

baseEvents :: Pattern -> [Tempo]
baseEvents (Pattern n w s) = every [from128 s + 1 / fromIntegral w / fromIntegral n * fromIntegral j | j <- [0 .. n - 1]] (`floatMod` 1)

higherEvents :: IntMap Presence -> Pattern -> [(Tempo,Int)]
higherEvents mp (Pattern n w s) = case M.lookup n mp of
        Nothing ->  []
        Just p -> map (_1 %~ (\x -> (from128 s + x / fromIntegral w) `floatMod` 1)) $ schedule mp p



schedule :: IntMap Presence -> Presence -> [(Tempo,Int)]
schedule _ (Base pre) = flip zip (repeat 1) . sort . concat $ every (M.elems pre) baseEvents  
schedule mp (Higher pre) = sort . concat $ every (M.elems pre) (higherEvents mp)

data Sequenza = Sequenza 
        { _pres :: Presence
        , _sample :: Int
        , _pitch :: Int -- pitch
        , _damp :: Int
        , _shift :: Int
        , _quant :: Int
        }
        deriving (Show,Read)

$(makeLenses ''Sequenza)

noseq l = Sequenza 
        (l (M.fromList $ zip [0..7] $ repeat (Pattern 0 1 0)))
        0
        64
        64
        0
        32


hystogram :: Double -> [(Tempo,Int)] -> [(Tempo,Int)]
hystogram m = map (fst . head &&& sum . map snd) . groupBy (collapse m `on` fst) . map (first $ quantize m) where
        collapse m x y = abs (x - y) < 1/m
        quantize m = (/m) . fromIntegral . floor . (*m) 

sequenza ::  IntMap Presence -> Sequenza -> [(Tempo,Double)]
sequenza mp (Sequenza p s pi da sh qm) = let 
        ts = hystogram (fromIntegral qm) $ schedule mp p
        m = maximum (map snd ts)
        in every ts $ \(t,n) ->  (floatMod (fromIntegral sh / fromIntegral qm + t) 1,from128 da * fromIntegral n/fromIntegral m)

unsafeMapLens :: Int -> Lens Presence Presence Pattern Pattern
unsafeMapLens n = lens f g where
        f (Base m) = m M.! n
        f (Higher m) = m M.! n
        g (Base m) v = Base (M.insert n v m)
        g (Higher m) v = Higher (M.insert n v m)

-- seq_lenses :: [ALens' Sequenza Int]
seq_lenses  :: Functor f => [(Int -> f Int) -> Sequenza -> f Sequenza]

seq_lenses = 
        [  pres . unsafeMapLens 0 . pnumber
        ,  pres . unsafeMapLens 1 . pnumber
        ,  pres . unsafeMapLens 2 . pnumber
        ,  pres . unsafeMapLens 3 . pnumber
        ,  pres . unsafeMapLens 4 . pnumber
        ,  pres . unsafeMapLens 5 . pnumber
        ,  pres . unsafeMapLens 6 . pnumber
        ,  pres . unsafeMapLens 7 . pnumber
        ,  pres . unsafeMapLens 0 . pwidth
        ,  pres . unsafeMapLens 1 . pwidth
        ,  pres . unsafeMapLens 2 . pwidth
        ,  pres . unsafeMapLens 3 . pwidth
        ,  pres . unsafeMapLens 4 . pwidth
        ,  pres . unsafeMapLens 5 . pwidth
        ,  pres . unsafeMapLens 6 . pwidth
        ,  pres . unsafeMapLens 7 . pwidth
        ,  pres . unsafeMapLens 0 . pshift
        ,  pres . unsafeMapLens 1 . pshift
        ,  pres . unsafeMapLens 2 . pshift
        ,  pres . unsafeMapLens 3 . pshift
        ,  pres . unsafeMapLens 4 . pshift
        ,  pres . unsafeMapLens 5 . pshift
        ,  pres . unsafeMapLens 6 . pshift
        ,  pres . unsafeMapLens 7 . pshift
        , sample, pitch, shift, damp, quant
        ]

