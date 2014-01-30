
{-# LANGUAGE TypeFamilies, TemplateHaskell #-}

-- module Sequencer where

import Sound.OSC (Time,time,pauseThreadUntil)
import Control.Monad (ap, forever, when,forM_, replicateM)
import Debug.Trace
import Control.Concurrent
import Control.Concurrent.STM

import Samples
import Sequencer
import MidiComm

import Control.Lens
import Control.Lens.TH

data Affine1

type instance State Affine1 = Integer

data AffineParams = AffineParams 
	
	{	_factor :: Integer
	,	_offset :: Integer
	,	_start :: Integer
	,	_hpf :: Integer
	}
$(makeLenses ''AffineParams)

type instance Params Affine1 = AffineParams

affineseq :: Int -> TVar Environment -> Time -> State Affine1 -> IO (Feedback Affine1)
affineseq c se t s = do
	Environment af v pe de sa <- atomically $ readTVar se
	playSample c sa (t + 0.2) 0.5 (v*fromIntegral s/128) (fromIntegral s/128 * 4) 
	return $ Feedback af (af ^. start) (de,128) pe

affinestep :: Step Affine1
affinestep v (AffineParams f o m h) = let 
	(n,v') = (f * v + o) `divMod` 128 -- new state and a multiplier
	k = max h . head $ dropWhile (<v') $ iterate (*2) 1
	in  trace (show ((n + 1,k),v',v)) $ ((((n `mod` 4) + 1) * k,1024),v')
what x = trace (show x) x

data Environment = Environment
	{	_affineparams :: AffineParams
	,	_volume	:: Double
	,	_periodo :: Double
	,	_delay :: Integer
	,	_sample :: Integer
	}
$(makeLenses ''Environment)
main = do	
	se <- replicateM 8 $ newTVarIO (Environment (AffineParams 1 0 64 0) 0 4 0 0)
	ls <- initSamples "/home/paolino/WAV/*/*/*.wav"

	let 	loop c s (Sequencer f) = f (affineseq c s) >>= loop c s
	forM_ (zip [0 ..] se) $ \(c,s) -> forkIO $ loop c s $  sequencer affinestep (Feedback (AffineParams 1 5 128 0) 0 (0,1) 4)
	mc <- newTChanIO 
	forkIO $ midiIn "samples" 0 mc
	forever $ do 
			atomically $ do
				(n,s) <- readTChan mc
				let (x,y) = n `divMod` 8
				when (x < 7) $ case x of 	
					6 -> modifyTVar (se !! y) $ sample .~ fromIntegral s
					0 -> modifyTVar (se !! y) $ affineparams .  factor .~ fromIntegral s
					1 -> modifyTVar (se !! y) $ affineparams .  offset .~ fromIntegral s
					2 -> modifyTVar (se !! y) $ affineparams .  start .~ (fromIntegral s + 1)
					--8 -> modifyTVar (se !! y) $ affineparams .  hpf .~ fromIntegral s
					3 -> modifyTVar (se !! y) $ volume .~ fromIntegral s/128
					5 -> modifyTVar (se !! y) $ delay .~ fromIntegral s * 8
					4 -> modifyTVar (se !! y) $ periodo .~ fromIntegral s/128*8
					--2 -> modifyTVar (td !! y) . const $ fromIntegral s 
					--3 -> modifyTVar (tv !! y) . const $ fromIntegral s / 128
					--4 -> modifyTVar (tq !! y) . const $ (fromIntegral s + 1) / 16
					--5 -> modifyTVar (tz !! y) . const $ fromIntegral s 
					_ -> return ()
					



