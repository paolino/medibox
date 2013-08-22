{-# LANGUAGE TemplateHaskell #-}       

module Projections (Projection, ampl, offset, shift, quant, cutin, project) where


import Control.Arrow (second, (***))
import Control.Lens 
import Data.Binary

import Haskell
import Sequenza


data Projection = Projection
        { _ampl :: Int
        , _offset :: Int
        , _shift :: Int
        , _quant :: Int
        , _cutin :: Int
        } 

$(makeLenses ''Projection)


instance Binary Projection where
  put (Projection a b c d e) = put a >> put b >> put c >> put d >> put e
  get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> return (Projection a b c d e)

project :: Score Int -> Projection -> Score Double
project sc (Projection da o sh qm ci') = let 
                        ci = from128 ci'
                        ts = hystogram (fromIntegral qm) $ sc
                        cut x = if x >= ci then x else 0
                        sht t = floatMod (from128 sh + t) 1
                        tsc ts = filter ((> 0) . snd) . every ts $ second cut
                        m = maximum $ map snd ts
                        in tsc . every ts $ sht *** ((from128 o +).(from128 da *). (/fromIntegral m) . fromIntegral)




