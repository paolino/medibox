{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, ViewPatterns, FlexibleInstances, Rank2Types #-}       

module Projections where

import Data.List (groupBy, partition, sort)
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
import Sequenza


data Projection = Projection
        { _psequenza :: Int
        , _ampl :: Int
        , _offset :: Int
        , _shift :: Int
        , _quant :: Int
        , _cutin :: Int
        } deriving (Read,Show)

$(makeLenses ''Projection)

projection_lenses :: Functor f => [(Int -> f Int) -> Projection -> f Projection]
projection_lenses = [ psequenza, ampl, offset, shift, quant, cutin]

project :: DSequenze -> Projection -> (Score Double, DSequenze)
project mp (Projection i da o sh qm ci') = let 
        r = querySequenza mp i
        in case r of
                Nothing -> ([],mp)
                Just (_,sc,mp') -> let 
                        ci = from128 ci'
                        ts = hystogram (fromIntegral qm) $ sc
                        cut x = if x >= ci then x else 0
                        sht t = floatMod (from128 sh + t) 1
                        tsc ts = filter ((> 0) . snd) . every ts $ second cut
                        m = maximum $ map snd ts
                        in first tsc $ (every ts $ sht *** ((from128 o +).(from128 da *). (/fromIntegral m) . fromIntegral), mp')


instance Present Projection where
        zero = Projection 0 64 0 0 32 0


