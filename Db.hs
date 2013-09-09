{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable, GADTs, FlexibleInstances #-}
module Db 
	( Db
	, tracks
	, synths 
	, Bus
	, Track , tseq, tproj, tgoal
	, Synth , sname, sparams
	, Param
	, Goal (..) 
	, I(..)
	) where

import Prelude hiding (filter)
import Data.Binary
import Data.Function (on)
import Data.Map

import Score
import Control.Lens
import Data.Default
import Data.List.Zipper hiding (empty)

data Goal = GBus (I Bus) | GSynth (I Synth) | GNothing 

instance Default Goal where
	def = GNothing

instance Binary Goal where
  put (GBus a) = putWord8 0 >> put a
  put (GSynth a) = putWord8 1 >> put a
  put GNothing = putWord8 2
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (GBus a)
      1 -> get >>= \a -> return (GSynth a)
      2 -> return GNothing
      _ -> fail "no parse"




data Synth = Synth
	{	_sname :: String
	,	_sparams :: Map (I Param) (I Bus)
	}

instance Binary Synth where
  put (Synth s p) = put s >> put p
  get = get >>= \s ->  get >>= \p -> return (Synth s p)

data Param
data Bus
data Track = Track 
	{	_tseq :: Sequenza
	,	_tproj :: Projection 
	, 	_tgoal :: Goal
	}


instance Binary Track where
  put (Track s p g) = put s >> put p >> put g
  get = get >>= \s ->  get >>= \p -> get >>= \g -> return (Track s p g)

instance Default Track where
	def = Track def def def

data I a where
	IT :: Int -> I Track
	IP :: Int -> I Param
	IB :: Int -> I Bus
	IS :: Int -> I Synth

fromI :: I a -> Int
fromI (IT a) = a
fromI (IP a) = a
fromI (IB a) = a
fromI (IS a) = a

instance Eq (I a) where
	(==) = (==) `on` fromI

instance Ord (I a) where
	compare = compare `on` fromI

instance Binary (I Track) where
  put (IT a) = put a
  get = get >>= \a ->  return (IT a)

instance Binary (I Param) where
  put (IP a) = put a
  get = get >>= \a ->  return (IP a)

instance Binary (I Bus) where
  put (IB a) = put a
  get = get >>= \a ->  return (IB a)

instance Binary (I Synth) where
  put (IS a) = put a
  get = get >>= \a ->  return (IS a)



data Db = Db 
	{ _tracks :: Map (I Track) Track
	, _synths :: Map (I Synth) Synth
	}

instance Binary Db where
	put (Db x y) = put x >> put y
	get = get >>= \x -> get >>= \y -> return (Db x y)

instance Default Db where
	def = Db empty empty


$(makeLenses ''Track)
$(makeLenses ''Synth)
$(makePrisms ''Goal)
$(makeLenses ''Db)





