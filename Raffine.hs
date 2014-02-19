
{-# LANGUAGE TypeFamilies, TemplateHaskell #-}

-- module Sequencer where

import Sound.OSC (Time,time,pauseThreadUntil)
import Control.Monad (ap, forever, when,forM_, replicateM, forM)
import Debug.Trace
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map as M

import Samples
import Bass
import Sequencer
import MidiComm

import Control.Lens
import Control.Lens.TH

import Data.Binary
data Raffine1

type instance State Raffine1 = [Integer]

rythm n m = zipWith subtract `ap` tail $  map (`div` n) [0,m ..]


update :: RaffineParams -> RaffineParams
update (RaffineParams n g m s) = RaffineParams n g m $ rythm n m
data RaffineParams = RaffineParams 
	
	{	_factor :: Integer
	,	_offset :: Integer
	,	_group :: Integer
	,	_start :: State Raffine1
	} deriving (Show)
$(makeLenses ''RaffineParams)

instance Binary RaffineParams where
	put (RaffineParams sh o p co) = put sh >> put o >> put p 
	get = do 
                sh <- get
		o <- get
		p <- get 
		return (RaffineParams sh o p $ rythm sh p)

data Environment = Environment
	{	_affineparams :: RaffineParams
	,	_volume	:: Double
	,	_periodo :: Double
	,	_delay :: Integer
	,	_sample :: Integer
	,	_unmuted :: Bool 
	,	_effects :: M.Map Int Double
	} deriving (Show)
instance Binary Environment where
	put (Environment sh o p co x m y) = put sh >> put o >> put p >> put co >> put x >> put m >>put y
	get = do  
                sh <- get
		o <- get
		p <- get
		co <- get
		x <- get
		m <- get
		y <- get
	
		return (Environment sh o p co x m y)

$(makeLenses ''Environment)
type instance Params Raffine1 = RaffineParams

affineseq :: Int -> TVar (Int,Double) ->  TVar Environment -> Time -> Time -> State Raffine1 -> IO (Feedback Raffine1)
affineseq c tap se t dt v' = do
	e@(Environment af v pe de sa m ef) <- atomically $ readTVar se
	when m $ do
		playSample c sa (t + 0.2) 0.5 (v*4*dt/pe) dt (ef) 3 1
	return $ Feedback af (af ^. start) (de,128) pe

affinestep :: Step Raffine1
affinestep (vs) (RaffineParams f o m h) = let 
	in   ((vs !! fromIntegral o ,m),tail vs)

what x = trace (show x) x

main = do	
  	ws <- decodeFile "current.bb"
	se <- forM ws $ newTVarIO
	-- se <- replicateM 8 $ newTVarIO (Environment (RaffineParams 7 0 12 $ rythm 7 12) 0 4 0 0 False $ M.fromList $ zip [0..] $ replicate 24 0)
	tap <- newTVarIO (3,1)
	let 	loop :: Int  -> TVar Environment -> Sequencer Raffine1 -> IO ()
		loop c s (Sequencer f) = f (affineseq c tap s) >>= loop c s
	forM_ (zip [0 ..] se) $ \(c,s) -> forkIO $ loop c s $  sequencer affinestep (Feedback (RaffineParams 7 0 12 $ rythm 7 12) (rythm 7 12) (0,1) 1)
	mc <- newTChanIO 
	forkIO $ midiIn "samples" 0 mc
	mcf <- newTChanIO 
	forkIO $ midiIn "samples_effect" 1 mcf


	fbinst <- newTChanIO
	
	tfbp <- newTChanIO 
  	forkIO $ midiOutNP ("samples_effect") 1 tfbp
	forkIO . forever .atomically $ do
		y <-  readTChan fbinst
		ps <- readTVar $ se !! y
		forM_ (M.assocs $ ps ^. effects) $ \(p,v) -> writeTChan tfbp (p,floor (v * 1000))
					
	tfbp2 <- newTChanIO 
	fbinst2 <- atomically $ dupTChan fbinst
  	forkIO $ midiOutNPx ("samples") 0 tfbp2
	forkIO . forever .atomically $ do
		y <-  readTChan fbinst2
		e@(Environment af v pe de sa m ef) <- readTVar (se !! y)
		writeTChan tfbp2 (24, floor $ v * 128,False)
		writeTChan tfbp2 (31, fromIntegral sa,True )
		writeTChan tfbp2 (25, fromIntegral de, False)
		writeTChan tfbp2 (26, fromIntegral $ af ^. offset,False)
		writeTChan tfbp2 (27, fromIntegral $ af ^. factor,False)
		writeTChan tfbp2 (28, fromIntegral $ af ^. group,False)
		writeTChan tfbp2 (29, fromIntegral $ floor (pe * 16) - 1,False)

	inst <- newTVarIO 0
	forkIO . forever . atomically $ do
				(n,s) <- readTChan mc
				y <- readTVar inst
				case n of 
					127 -> do 
						writeTVar inst $ fromIntegral s
						writeTChan fbinst $ fromIntegral s	
					24 -> modifyTVar (se !! y) $ volume .~ fromIntegral s/128
					29 -> modifyTVar (se !! y) $ periodo .~ (fromIntegral (s + 1)/ 16)
					31 -> modifyTVar (se !! y) $ sample .~ fromIntegral s
					25 -> modifyTVar (se !! y) $ delay .~ fromIntegral s
					26 -> modifyTVar (se !! y) $ affineparams .  offset .~ fromIntegral s
					27 -> modifyTVar (se !! y) $ (affineparams %~ update) . (affineparams .  factor .~ (fromIntegral s))
					28 -> modifyTVar (se !! y) $ (affineparams %~ update) . (affineparams .  group .~ (fromIntegral s))
					_ -> when (n >= 112 && n <=119) $ case s of
						127 -> do 
							modifyTVar (se !! (n - 112)) $ unmuted .~ True
							writeTVar inst $ fromIntegral (n - 112)
							writeTChan fbinst $ fromIntegral (n - 112)	
						0 -> modifyTVar (se !! (n -112)) $ unmuted .~ False

					
	forkIO . forever . atomically $ do
				(n,s) <- readTChan mcf
				y <- readTVar inst
				modifyTVar (se !! y) $ effects  %~ (flip M.adjust n $ \_ ->  fromIntegral s/1000)
 	threadDelay 1000000	
	atomically $ writeTChan fbinst $ 0
	forM_ (zip [0..] se) $ \(n,se) -> do
		s <- atomically $ readTVar se	
		case s ^. unmuted of
			True -> atomically $ writeTChan tfbp2 (n + 112,127, False)
			False -> atomically $ writeTChan tfbp2 (n+ 112,0, False)
	getLine
	ws <- atomically $ mapM readTVar se
		
 	encodeFile "current.bb" ws

	



