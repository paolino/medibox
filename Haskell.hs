
{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, ViewPatterns, FlexibleInstances, Rank2Types #-}       

module Haskell where

floatMod x y = let
        r = x/y
        in r - fromIntegral (floor r) * y

every = flip map



