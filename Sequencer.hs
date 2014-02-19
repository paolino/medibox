{-# LANGUAGE TypeFamilies #-}

-- module Sequencer where

import Sound.OSC (Time,time,pauseThreadUntil)
import Control.Monad (ap)
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
	       colpi :: [Beat] -- unsorted, finite
        ,	late :: Beat
	,	window :: Event
	}

data Event = Event {
        when :: Time
        , dur :: Period
        } deriving Show

-- make a sequencer from a concurrent sequencing environment
sequencer :: Time -> Sequencer 
sequencer = Sequencer . g where
	g t f (Config cs l (Event t0 q)) = do
		nq <- quantize t0 q
		let     t' = head $ dropWhile (<=t) . sort $ (nq - q:nq + q:nq : map (((nq - q) +) . fromBeat q) cs)
                        t'' = fromBeat q l + t
		f  $ Event t'' (t' - t)
		pauseThreadUntil t'
		return . Sequencer $ g t' 

adriver :: IO Config -> (Event -> IO ()) -> Time -> IO ()
adriver ic f = loop . sequencer where
                loop (Sequencer g) = ic >>= g f >>= loop
        
        
main = do
        t <- time
        c1 <- newTVarIO $ Config [(1,2),(1,4)] (0,1) (Event t 2)
        c2 <- newTVarIO $ Config [(1,3),(2,3)] (0,1) (Event t 2)
        forkIO $ adriver (atomically $ readTVar c2) print t
        adriver (atomically $ readTVar c1) (\x -> atomically (modifyTVar c2 (\(Config cs l _) -> Config cs l x))) t



