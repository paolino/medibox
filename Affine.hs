
{-# LANGUAGE TypeFamilies, TemplateHaskell #-}

-- module Sequencer where

import Sound.OSC (Time,time,pauseThreadUntil)
import Control.Monad (ap, forever, when,forM_, replicateM, forM)
import Debug.Trace
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map as M

import Samples
import Sequencer
import MidiComm

import Control.Lens
import Control.Lens.TH

import Data.Binary
data Affine1

type instance State Affine1 = Integer

data AffineParams = AffineParams 
	
	{	_factor :: Integer
	,	_offset :: Integer
	,	_start :: Integer
	,	_hpf :: Integer
	}
$(makeLenses ''AffineParams)
instance Binary AffineParams where
	put (AffineParams sh o p co) = put sh >> put o >> put p >> put co
	get = do 
                sh <- get
		o <- get
		p <- get
		co <- get
		return (AffineParams sh o p co)

data Environment = Environment
	{	_affineparams :: AffineParams
	,	_volume	:: Double
	,	_periodo :: Double
	,	_delay :: Integer
	,	_sample :: Integer
	,	_effects :: M.Map Int Double
	}
instance Binary Environment where
	put (Environment sh o p co x y) = put sh >> put o >> put p >> put co >> put x >> put y
	get = do 
                sh <- get
		o <- get
		p <- get
		co <- get
		x <- get
		y <- get
		return (Environment sh o p co x y)

$(makeLenses ''Environment)
type instance Params Affine1 = AffineParams

affineseq :: Int -> TVar Environment -> Time -> State Affine1 -> IO (Feedback Affine1)
affineseq c se t s = do
	Environment af v pe de sa ef <- atomically $ readTVar se
	playSample c sa (t + 0.2) 0.5 (v*fromIntegral s/128) (fromIntegral s/128 * 4) (ef)
	return $ Feedback af (af ^. start) (de,128) pe

affinestep :: Step Affine1
affinestep v (AffineParams f o m h) = let 
	(n,v') = (f * v + o) `divMod` 128 -- new state and a multiplier
	k = max h . head $ dropWhile (<v') $ iterate (*2) 1
	in   ((((n `mod` 4) + 1) * k,1024),v')
what x = trace (show x) x

main = do	
  	ws <- decodeFile "current.bb"
	se <- forM ws $ newTVarIO
	-- se <- replicateM 8 $ newTVarIO (Environment (AffineParams 1 0 64 0) 0 4 0 0 $ M.fromList $ zip [0..] $ replicate 24 0)
	-- ls <- initSamples "/home/paolino/WAV/*/*/*.wav"
	
	let 	loop :: Int  -> TVar Environment -> Sequencer Affine1 -> IO ()
		loop c s (Sequencer f) = f (affineseq c s) >>= loop c s
	forM_ (zip [0 ..] se) $ \(c,s) -> forkIO $ loop c s $  sequencer affinestep (Feedback (AffineParams 1 5 128 0) 0 (0,1) 4)
	mc <- newTChanIO 
	forkIO $ midiIn "samples" 0 mc
	mcf <- newTChanIO 
	forkIO $ midiIn "samples_effect" 1 mcf


	tfbp <- newTChanIO 
	fbinst <- newTChanIO
  	forkIO $ midiOutNP ("samples_effect") 1 tfbp
	forkIO . forever .atomically $ do
		y <-  readTChan fbinst
		ps <- readTVar $ se !! y
		forM_ (M.assocs $ ps ^. effects) $ \(p,v) -> writeTChan tfbp (p,floor (v * 1000))
		

	inst <- newTVarIO 0
	forkIO . forever . atomically $ do
				(n,s) <- readTChan mc
				let (x,y) = n `divMod` 8
				y' <- readTVar inst
				writeTVar inst y
				when (y /= y') $ writeTChan fbinst y
				when (x < 7) $ case x of 	
					6 -> modifyTVar (se !! y) $ sample .~ fromIntegral s
					0 -> modifyTVar (se !! y) $ affineparams .  factor .~ fromIntegral s
					1 -> modifyTVar (se !! y) $ affineparams .  offset .~ fromIntegral s
					2 -> modifyTVar (se !! y) $ affineparams .  start .~ (fromIntegral s + 1)
					3 -> modifyTVar (se !! y) $ volume .~ fromIntegral s/128
					5 -> modifyTVar (se !! y) $ delay .~ fromIntegral s * 8
					4 -> modifyTVar (se !! y) $ periodo .~ (fromIntegral (2 ^ fromIntegral s) / 16)
					_ -> return ()
					
	forkIO . forever . atomically $ do
				(n,s) <- readTChan mcf
				
				y <- trace (show (n,s)) $ readTVar inst
				modifyTVar (se !! y) $ effects  %~ (flip M.adjust n $ \_ ->  fromIntegral s/1000)

	getLine
	ws <- atomically $ mapM readTVar se
		
 	encodeFile "current.bb" ws

	



