{-# LANGUAGE TypeFamilies, TemplateHaskell, Rank2Types #-}

module Euclidean.Data where

import Control.Lens
import Control.Lens.TH
import qualified Data.Map as M
import Data.Binary

seque k ls = case filter (>0) ls of
		[] -> []
		ls -> takeWhile (<1) $ scanl (+) 0 $ drop k $ cycle ls

---------------- Euclidean ------------------------------
data Euclidean = Euclidean 
	{	_factor :: [Double]
	,	_offset :: Integer
	,	_group :: Integer
	,	_delay :: Integer
	}
        deriving (Show)	
$(makeLenses ''Euclidean)
instance Binary Euclidean where
	put (Euclidean x1 x2 x3 x4) = put x1 >> put x2 >> put x3 >> put x4  
	get = do  
                x1 <- get

		x2 <- get
		x3 <- get
		x4 <- get
		return (Euclidean x1 x2 x3 x4)

data EuclideanField = Factor [Double] | Offset Integer | Sub Integer | Delay Integer 
        deriving (Show,Read)
setRythmField (Factor i)  =  set  ( factor ) i
setRythmField (Offset i)  =  set  ( offset ) i
setRythmField (Sub i)  =  set ( group ) i
setRythmField (Delay i)  =  set ( delay ) i

----------------------------------------------------

---------------- Instrument -----------------------
data Instrument = Instrument
	{	_volume	:: Double
	,	_sample :: Integer
	,	_unmuted :: Bool 
	,	_effects :: M.Map Int Double
	} deriving (Show)

$(makeLenses ''Instrument)
instance Binary Instrument where
	put (Instrument x1 x2 x3 x4 ) = put x1 >> put x2 >> put x3 >> put x4 	
	get = do  
                x1 <- get
		x2 <- get
		x3 <- get
		x4 <- get	
		return (Instrument x1 x2 x3 x4)

data InstrumentFieled =  Volume Double | Sample Integer | Unmuted Bool | Effect Int Integer
        deriving (Show,Read)

setInstrField (Volume i)  =  set ( volume ) i
setInstrField (Sample i)  =  set ( sample ) i
setInstrField (Unmuted i)  =  set ( unmuted ) i
setInstrField (Effect i x)  =  over ( effects ) (M.adjust (const $ fromIntegral x/1000) i)

