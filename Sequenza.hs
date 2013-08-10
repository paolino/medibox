{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, ViewPatterns, FlexibleInstances, Rank2Types #-}       

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
       
data Presence = Base {_pattern :: IntMap Pattern} | Higher {_pattern :: IntMap Pattern} deriving (Show,Read)

baseEvents :: Pattern -> [Tempo]
baseEvents (Pattern n w s) = every [from128 s + 1 / fromIntegral w / fromIntegral n * fromIntegral j | j <- [0 .. n - 1]] (`floatMod` 1)

higherEvents :: IntMap Presence -> Pattern -> [(Tempo,Int)]
higherEvents mp (Pattern n w s) = case M.lookup n mp of
        Nothing ->  []
        Just p -> map (_1 %~ (\x -> (from128 s + x / fromIntegral w) `floatMod` 1)) $ schedule mp p


schedule :: IntMap Presence -> Presence -> [(Tempo,Int)]
schedule _ (Base pre) = flip zip (repeat 1) . sort . concat $ every (M.elems pre) baseEvents  
schedule mp (Higher pre) = sort . concat $ every (M.elems pre) (higherEvents mp)

hystogram :: Double -> [(Tempo,Int)] -> [(Tempo,Int)]
hystogram m = map (fst . head &&& sum . map snd) . groupBy (collapse m `on` fst) . map (first $ quantize m) where
        collapse m x y = abs (x - y) < 1/m
        quantize m = (/m) . fromIntegral . floor . (*m) 

pattern :: IntMap Presence -> Int -> Int -> [(Tempo,Int)]
pattern mp qm i = hystogram (fromIntegral qm) $ schedule mp (mp M.! i)

unsafeMapLens :: Int -> Lens Presence Presence Pattern Pattern
unsafeMapLens n = lens f g where
        f (Base m) = m M.! n
        f (Higher m) = m M.! n
        g (Base m) v = Base (M.insert n v m)
        g (Higher m) v = Higher (M.insert n v m)


presence_lenses = 
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

instance Present Pattern where
        zero = Pattern 0 1 0

instance Present Presence where
        zero = Base $ M.fromList $ zip [0..7] $ repeat zero

-----------------------------------------------------------

type PM = IntMap Presence

-- check deplist correctness
-- a dep list is correct if it has no cycles, it forms an acyclic graph
-- a dep is represented by an ordered tuple, first element is the dependent
                
correct [] = True
correct ys = correct' [Nothing] ys

depsOfPresence (Base ps) = [Nothing]
depsOfPresence (Higher ps) = every (M.elems ps) $ \(Pattern n _ _) -> Just n

checkPM :: PM -> Bool
checkPM = correct . concatMap (uncurry zip . (repeat . Just *** depsOfPresence)) . M.assocs 

pointing :: Eq a => a -> [(a,a)] -> [a]
pointing x [] = []
pointing x ys = let 
        (map fst -> zs,ts) = partition ((==x) . snd) ys
        in zs ++ concatMap (flip pointing ts) zs

roots :: Eq a => [(a,a)] -> [a]
roots zs = let 
        (xs,ys) = unzip zs
        in nub ys \\ xs
       
acyclic :: Eq a =>  [(a,a)] -> Bool
acyclic xs = let 
        zs = roots xs
        in acyclicFrom zs xs

acyclicFrom xs ys = let 
        (ms,us) = partition ((`elem` xs) . snd)  ys
        in case us of 
                [] -> True
                us -> case map fst us `intersect` xs of 
                        [] ->  case ms of 
                                [] -> False 
                                ms -> acyclicFrom (map fst ms ++ xs) us
                        _ -> False

