{-# LANGUAGE TypeFamilies #-}
module Event where

import Sound.OSC (Time,time,pauseThreadUntil)
import Control.Monad (ap, forM_ , forever)
import Debug.Trace
import Data.List
import Control.Concurrent
import Control.Concurrent.STM

-- time difference
type Period = Time

-- find next beat from 0 time
quantizeM :: Time -> Period -> IO Time
quantizeM t0 pe = (t0 +) `fmap`  quantize t0 pe `fmap` time


quantize :: Time -> Time -> Time  -> Time
quantize t0 pe dt =  (fromIntegral . ceiling $ (dt - t0)/pe) * pe

-- fraction of a measure
type Beat = (Integer,Integer)

-- actual subperiod
fromBeat :: Period -> Beat -> Period 
fromBeat q (n,k) = (fromIntegral n * q / fromIntegral k)


-- sequencer interface
data Config = Config {
	        colpi :: [Beat]  -- sorted, finite timing for response
        ,	late :: Beat     -- initial delay
	}

data Event a = Event 
        {       _index :: a        -- producer
        ,       _when :: Time           -- timestamp
        ,       _dur :: Period          -- duration
        } deriving Show


mkEvents :: Time -> Time -> a -> Config -> [Event a]
mkEvents w d j (Config cs l) = let 	
        ts = zip cs $ (tail cs ++ [(1,1)])
	in map (\(c1,c2) -> Event j (w + fromBeat d l + fromBeat d c1) (fromBeat d c2 - fromBeat d c1)) ts

	
        


