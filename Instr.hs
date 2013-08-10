{-# LANGUAGE TemplateHaskell #-}
module Instr where

import Control.Lens
import Language.Haskell.TH.Lens


data SSample  a = SSample
        { _volume :: a
        , _pitch :: a
        , _sample :: Int
        , _soutbus :: Int
        }
        deriving (Show,Read)

$(makeLenses ''SSample)

data SControllo a = SControllo
        { _cfilter :: Int
        , _cparam :: Int
        , _cvalue :: a
        }
        deriving (Show,Read)
$(makeLenses ''SControllo)

data SBus = SBus 
        { _boutbus :: Int
        , _binbus :: Int
        }
        deriving (Show,Read)


$(makeLenses ''SBus)

baseSBus = SBus 127 0
baseSSample = SSample baseProjection baseProjection 0 0
baseSControllo = SControllo 0 0 baseProjection 


ssample_lenses :: Functor f => [(Int -> f Int) -> SSample Projection -> f (SSample Projection)]
ssample_lenses =
        map (volume .) projection_lenses ++ [nolens,nolens,sample] ++ map (pitch .) projection_lenses ++ [nolens, nolens,soutbus] 
scontrollo_lenses :: Functor f => [(Int -> f Int) -> SControllo Projection -> f (SControllo Projection)]
scontrollo_lenses = map (cvalue .) projection_lenses ++ [nolens, cfilter, cparam] 

sbus_lenses = [binbus,boutbus]

