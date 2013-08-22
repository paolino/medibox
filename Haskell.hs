
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
        in r - fromIntegral (floor r) * y

every = flip map



from128 x = 1/128 * fromIntegral x

hystogram :: (Num d, RealFrac b) => b -> [(b, d)] -> [(b, d)]
hystogram m = map (fst . head &&& sum . map snd) . groupBy (collapse m `on` fst) . map (first $ quantize m) where
        collapse m x y = abs (x - y) < 1/m
        quantize m = (/m) . fromIntegral . floor . (*m) 


mapAccumM f s xs = let 
        swap (x,y) = (y,x) 
        f' x s = swap `fmap` f s x
        in swap `fmap`  runStateT (mapM (StateT . f') xs) s 
