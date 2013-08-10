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


data Projection = Projection
        { _pres :: Int
        , _ampl :: Int
        , _offset :: Int
        , _shift :: Int
        , _quant :: Int
        , _cutin :: Int
        } deriving (Read,Show)

$(makeLenses ''Projection)

projection_lenses :: Functor f => [(Int -> f Int) -> Projection -> f Projection]
projection_lenses = [ pres, ampl, offset, shift, quant, cutin]

project :: IntMap Presence -> Projection -> [(Tempo,Double)]
project mp (Projection i da o sh qm ci) = let 
        p = mp M.! i
        ts = hystogram (fromIntegral qm) $ schedule mp p
        cut x = if x >= ci then x else 0
        sht t = floatMod (fromIntegral sh / fromIntegral qm + t) 1
        tsc = filter ((> 0) . snd) . every ts $ second cut
        m = maximum (map snd tsc)       
        tsc' = every tsc $ second ((/(m - ci)) . (subtract ci))
        in every tsc' $ sht *** (from128p o +)
        m = maximum (map snd ts)        


instance Present Projection where
        zero = Projection 0 0 64 0 16

