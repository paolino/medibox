
{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}       

module PersistentChoiceLens (persistentChoice, ABC (..), nextChoice, choice , PersistentChoice, PInt (..),Present (..)) where

import Data.Monoid
import Control.Lens
import Data.List
import Haskell

data ABC a      = A { _selection :: a} 
                | B { _selection :: a}
                | C { _selection :: a}
                | D { _selection :: a}
        deriving (Eq,Ord,Show,Read)


$(makeLenses ''ABC)
newtype PersistentChoice a = PersistentChoice {_choices :: [ABC a]} deriving Show
        
$(makeLenses ''PersistentChoice)

persistentChoice :: forall a f. (Ord a, Present a, Functor f) => (ABC a -> f (ABC a)) -> PersistentChoice a -> f (PersistentChoice a)
persistentChoice  = lens (\ps ->  head (ps ^. choices)) $ \(PersistentChoice y) x -> PersistentChoice $ x : (map snd . filter  ((/=) (selection .~ zero $ x) . fst) 
                                        $ zip [A zero :: ABC a, B zero, C zero, D zero] (sort y)
                                        )
instance (Ord a, Present a) => Present (PersistentChoice a) where
        zero = PersistentChoice [A zero , B zero, C zero,D zero]

nextChoice :: PersistentChoice a -> PersistentChoice a
nextChoice (PersistentChoice xs) = PersistentChoice (take 4 $ tail $ cycle xs)

choice = persistentChoice . selection

newtype PInt = PInt Int  deriving (Num, Ord, Eq, Show, Read, Integral, Real, Enum)

instance Present PInt where
        zero = 0
