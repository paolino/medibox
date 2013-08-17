{-# LANGUAGE TemplateHaskell, TypeFamilies, FlexibleInstances #-}
module Instr where

import Control.Lens
import Language.Haskell.TH.Lens
import Projections
import Haskell
import Realize



data SSample a = SSample 
        { _volume :: a
        , _pitch :: a
        , _sample :: Int
        , _sinbus :: Int
        , _soutbus :: Int
        }
        deriving (Show,Read)

$(makeLenses ''SSample)
ssample_lenses :: Functor f => [(Int -> f Int) -> (SSample Int) -> f (SSample Int)]
ssample_lenses = [volume,pitch,sample,soutbus]

instance Present (SSample Int) where
        zero = SSample 0 0 0 0 0

instance Realize (SSample Int) where 
        type EventR (SSample Int) = SSample Double
        realize pres (SSample vo pi sa ib ob) = do 
                vos <- pres vo
                pis <- pres pi
                let     f _ [] _ = []
                        f pi ((tvo,vo):vos) [] = Event tvo (SSample vo  pi sa ib ob): f pi vos []
                        f pi' ((tvo,vo):vos) ((tpi,pi):pis) 
                                | tvo < tpi = Event tvo (SSample vo pi sa ib ob): f pi' vos ((tpi,pi):pis)
                                | otherwise = f pi ((tvo,vo):vos) pis
                return $ case pis of
                        [] -> []
                        pis -> f (snd $ last pis) vos pis
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



