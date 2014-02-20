{-# LANGUAGE TypeFamilies #-}
module Sequencer where

import Sound.OSC (Time,time,pauseThreadUntil)
import Control.Monad (ap, forM_ , forever)
import Debug.Trace
import Data.List
import Control.Concurrent
import Control.Concurrent.STM

-- time difference
type Period = Time

-- find next beat from 0 time
quantize :: Time -> Period -> IO Time
quantize t0 pe = (t0 +) `fmap` (\dt ->  (fromIntegral . ceiling $ (dt - t0)/pe) * pe) `fmap` time

-- fraction of a measure
type Beat = (Integer,Integer)

-- actual subperiod
fromBeat :: Period -> Beat -> Period 
fromBeat q (n,k) = (fromIntegral n * q / fromIntegral k)


-- sequencer interface
data Config = Config {
		mask :: Integer, -- which producer to sync
	        colpi :: [Beat]  -- sorted, finite timing for response
        ,	late :: Beat     -- initial delay
	}

data Event = Event 
        {       _index ::Integer        -- producer
        ,       _when :: Time           -- timestamp
        ,       _dur :: Period          -- duration
        } deriving Show



sequencer 
        :: Integer     -- sequencer index
        -> TChan Event -- duplex even channel to peek and poke
        -> STM Config  -- a live config
        -> IO ()       -- loop forever 
sequencer j te tc = do
	md <- atomically $ do
		Event i w d <- readTChan te
		Config m cs l <- tc
		if i == m  then return $ Just (cs,l,w,d) else return Nothing
	case md of 
		Nothing -> sequencer j te tc
		Just (cs,l,w,d) -> do
			let 	ts = zip cs $ (tail cs ++ [(1,1)])
				es = map (\(c1,c2) -> Event j (w + fromBeat d l + fromBeat d c1) (fromBeat d c2 - fromBeat d c1)) ts
			atomically $ mapM_ (writeTChan te) es
			pauseThreadUntil $ w + d
			sequencer j te tc
		
	
        


