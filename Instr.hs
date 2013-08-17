{-# LANGUAGE TemplateHaskell, TypeFamilies, FlexibleInstances #-}
module Instr where

import Control.Lens
import Language.Haskell.TH.Lens
import Projections
import Haskell
import Realize
import Control.Monad


data Synth = Synth 
        {  _synth :: Int
        , _sinbus :: Int
        , _soutbus :: Int
        , _p1   :: Int
        , _p2 :: Int
        , _p3 :: Int
        , _p4 :: Int
        , _p5 :: Int
        , _p6 :: Int
        }
        deriving (Show,Read)

$(makeLenses ''Synth)

data SControl = SControl {
        _coutbus :: Int
        }
        deriving (Show,Read)

$(makeLenses ''SControl)

        


synth_lenses :: Functor f => [(Int -> f Int) -> Synth -> f Synth ]
synth_lenses = [synth, sinbus , soutbus,p1,p2,p3,p4,p5,p6]

instance Present a => Present (Realize a) where
        zero = Realize (0,0) $ zero

instance Present Synth where
        zero = Synth  0 0 0 0 0 0 0 0 0

instance Present SControl where
        zero = SControl 0

{-
data SControllo a = SControllo
        { _cfilter :: Int
        , _cvalue1 :: a
        , _cvalue2 :: a
        , _boutbus :: Int
        , _binbus :: Int
        }
        deriving (Show,Read)
$(makeLenses ''SControllo)

instance Present (SControllo Int) where
        zero = SControllo 0 0 0 4 0



instance Realize SControllo where
        realize pres (SControllo ef par va) = map (\(t,v) -> Event t (SControllo ef par v)) $ project pres va 
-}



