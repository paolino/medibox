
{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}       

module Haskell  where

import Data.Monoid
import Control.Lens
import Data.List
import qualified Data.IntMap 
import Control.Arrow
import Control.Monad.State
import Data.Function (on)

floatMod x y = let
        r = x/y
        in x - fromIntegral (floor r) * y

every = flip map




hystogram :: RealFrac b => b -> [b] -> [(b, Int)]
hystogram m = map (head &&& length) . groupBy (collapse m) . map (quantize m) . sort where
        collapse m x y = abs (x - y) < 1/m
        quantize m = (/m) . fromIntegral . floor . (*m) 

normalize :: [Double] -> [Double]
normalize xs = let
        m = maximum $ map abs xs
        in map (/m) xs

mapAccumM f s xs = let 
        swap (x,y) = (y,x) 
        f' x s = swap `fmap` f s x
        in swap `fmap`  runStateT (mapM (StateT . f') xs) s 
