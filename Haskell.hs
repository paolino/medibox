
{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}       

module Haskell (floatMod, every, Present (..)) where

import Data.Monoid
import Control.Lens
import Data.List
import qualified Data.IntMap 

floatMod x y = let
        r = x/y
        in r - fromIntegral (floor r) * y

every = flip map

class Present a where
        zero :: a

instance Present a => Present (Data.IntMap.IntMap a) where
        zero = Data.IntMap.singleton 0 zero
