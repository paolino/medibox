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

newtype Sequencer = Sequencer ((Event -> IO ()) -> Config -> IO Sequencer)

-- sequencer interface
data Config = Config {
		mask :: Integer,
	       colpi :: [Beat] -- unsorted, finite
        ,	late :: Beat
	}

data Event = Event {
	_index ::Integer
        , _when :: Time
        , _dur :: Period
        } deriving Show

sequencer :: (a -> Config) -> Integer -> TChan Event -> TVar a -> IO ()
sequencer q j te tc = do
	md <- atomically $ do
		Event i w d <- readTChan te
		Config m cs l <- q `fmap` readTVar tc
		if i == m  then return $ Just (cs,l,w,d) else return Nothing
	case md of 
		Nothing -> sequencer q j te tc
		Just (cs,l,w,d) -> do
			let 	ts = zip cs $ (tail cs ++ [(1,1)])
				es = map (\(c1,c2) -> Event j (w + fromBeat d l + fromBeat d c1) (fromBeat d c2 - fromBeat d c1)) ts
			atomically $ mapM_ (writeTChan te) es
			pauseThreadUntil $ w + d
			sequencer q j te tc
		
	
        
{-
main = do
        t <- time
	te <- newTChanIO 
        c1 <- newTVarIO $ Config 0 [(0,1), (1,4),(1,2)] (0,1) 
	te1 <- atomically $ dupTChan te
        c2 <- newTVarIO $ Config 1 [(0,1), (1,3),(2,3)] (0,1) 
	te2 <- atomically $ dupTChan te
	atomically $ 	forM_ [0..15] $ \v -> writeTChan te (Event 0 (t + v * 2) 2)
	forkIO . forever $ do
		e <- atomically $ readTChan te
		print e
	forkIO $ sequencer 1 te1 c1
	sequencer 2 te2 c1 
-}


