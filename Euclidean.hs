
{-# LANGUAGE TypeFamilies, TemplateHaskell, Rank2Types #-}

-- module Sequencer where

import Sound.OSC (Time,time,pauseThreadUntil)
import Control.Monad (ap, forever, when,forM_, replicateM, forM, liftM2)
import Debug.Trace
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map as M
import Control.Monad.Trans
import Samples
-- import Sequencer
import MidiComm

import Control.Lens
import Control.Lens.TH

import Data.Binary
import System.Console.Haskeline
import Data.List.Zipper
import EuclideanData

data Select = Rythm Int | Instr Int  deriving (Show,Read)

mkConfig :: Euclidean -> ([Beat],Beat)
mkConfig (Euclidean fa ofs gr de) = (map (flip (,) gr . floor . (* fromIntegral gr)) $ seque (fromIntegral ofs) fa, (de,gr))

			let 	ts = zip cs $ (tail cs ++ [(1,1)])
				es = map (\(c1,c2) -> Event j (w + fromBeat d l + fromBeat d c1) (fromBeat d c2 - fromBeat d c1)) ts


render :: Int ->  Instrument -> (Time,Period,Period) -> IO ()
render c (Instrument v sa m ef) (t,dt,dt') = when m $ playSample c sa (t + 0.2) 0.5 (4*v*dt') dt (ef) 3 1


quant :: Time -> Time -> Time
quant pe dt =  fromIntegral . ceiling $ (dt - t0)/pe) * pe

selectGenerator :: M.Map Int F -> (Int,Time,Time)
selectGenerator t0 m = head . sort . catMaybes . map k $ M.assocs m where
        k (i,Generator t) = Just (i,t,quant t t0)
        k _ = Nothing

effes :: Zipper (Graph F) -> M.Map Int F
effes zg = fmap (view $ _2 . object) $ cursor zg ^. vertexes

nextTick :: Zipper (Graph F) -> IO (Event Int)
nextTick g = let 
        (i,dt,t) = selectGenerator (effes g)
        in pauseThreadUntil t >> return (Event i t dt)

main = do
        t <- time
	te <- newTChanIO  :: IO (TChan (Event Dependence))
	-- (cs',es') <- decodeFile "current.bb"
        let pa0 = Object (
	graph <- newTVarIO $ flip insert empty $ Graph (M.fromList $ 
		[ (0,(Affine (0.5,0.5) (0.1,0.06),basePattern))
		, (1,(Affine (0.5,0.5) (0.06,0.1),baseSound))
		, (2,(Affine (0.5,0.5) (0.06,0.06),baseGenerator))
		]) M.empty M.empty

        	
	-- master tempo 
	forkIO . forever $ do
                e <- atomically (readTVar graph) >>= nextTick
                atomically $ writeTChan te e

	-- euclidean replicators
	
	mj <- newTVarIO (M.fromList $ zip [0..ntrack - 1] $ repeat []) :: IO (TVar (M.Map  Int [Dependence]))
	forM_ (zip [0..] cs) $ \(n,c) -> do
		te' <- atomically $ dupTChan te
		let act :: STM (Config Dependence)
		    act = do
			rs <- flip (M.!) n `fmap`   readTVar mj
			(z,rs') <- mkConfig rs `fmap` readTVar c :: STM (Config Dependence,[Dependence])
			modifyTVar mj $ M.adjust (\_ -> rs') n
			return z
		forkIO $ sequencer (Up n) te' act 

        -- players
	mj' <- newTVarIO (M.fromList $ zip [0..ntrack - 1] $ repeat 0) -- remember last dt
	let loop m = do
		Event j t dt <- readTChan te
                g <- fmap cursor $ readTVar graph
                let     rit m (i,Sound e)   = let
                                dt' = maybe 0 $ M.lookup i m
                                m' = M.insert i dt m
                                cb =  do
                                        pauseThreadUntil t 
                                        render i e  (t,dt,dt')

                        rit m (i,Patern e) = 
                                        
                        (m',cbs) = mapAccumL rit m 
		forM_ (zip [0..] es) $ \(i,te) -> do
                                e <- atomically $ readTVar te
                                when (e ^. clock == [j]) $ do
                                        dt' <- flip (M.!) i `fmap` atomically (readTVar mj')
                                        atomically $ modifyTVar mj' $ M.adjust (const dt) i
                                        render (fromIntegral i) (e ^. element) (t,dt,dt')
        

                                                
	-------------- interface -----------------------

	ti <- newTVarIO $ 0
	tr <- newTVarIO $ 0
	trd <- newTVarIO $ 0
	tl <- newTVarIO $ Rythm 0

	tcsel <- newTChanIO
	midi cs es ti tr tcsel

	let input = do
                Just l <- getInputLine "> "
                case l of
                        "end" -> return ()
			_ -> do 
				liftIO $ case reads l of
					[(Rythm i,_)] -> do 
						atomically $ writeTVar tr $  i `mod` ntrack
						atomically $ writeTVar tl (Rythm $ i `mod` ntrack)
						atomically $ writeTChan tcsel (Rythm $ i `mod` ntrack)
						(atomically $ readTVar $ cs !! i) >>= print 
					[(Instr i,_)] -> do 
						atomically $ writeTVar ti $  i `mod` ntrack
						atomically $ writeTVar tl (Instr $ i `mod` ntrack)
						atomically $ writeTChan tcsel (Instr $ i `mod` ntrack)
						(atomically $ readTVar $ es !! i) >>= print 

					_ -> case reads l of
						[(c ,_)] -> do 
							atomically $ do 
								i <- readTVar tl
								case i of 
									Rythm i -> modifyTVar (cs !! i) $ setClockedField c
									Instr i -> modifyTVar (es !! i) $ setClockedField c

						_ -> case reads l of
							[(c,_)] -> do 
								i <- atomically $ do 
									i <- readTVar tr      
									modifyTVar (cs !! i) $ setRythmField c
									return i
								(atomically $ readTVar $ cs !! i) >>= print 
							_ -> case reads l of
								[(c,_)] -> do 
									i <-  atomically $ do 
										i <- readTVar ti
										modifyTVar (es !! i) $ setInstrField c
										return i
									(atomically $ readTVar $ es !! i) >>= print 
								_ ->  putStrLn "<< parsing failed >>" 
				input
	forkIO $ run (const return) (\_ _ -> return) renderSynth 
        runInputT defaultSettings input

	---------------- save work -------------------
 	cs' <- atomically $ mapM readTVar cs
 	es' <- atomically $ mapM readTVar es
		
 	encodeFile "current.bb" (cs',es')


	
midi :: [TVar (Clocked Euclidean)] -> [TVar (Clocked Instrument)] -> TVar Int  -> TVar Int -> TChan Select -> IO () 
midi cs es ti tr fbsel = 	do
	mci <- midiIn "euclidean" 0 
	mco <- midiOut ("euclidean") 0 
	forkIO . forever .atomically $ do
		y <-  readTChan fbsel
		case y of 
			Instr _ -> return ()
			Rythm y -> do 
				ce <- readTVar $ cs !! y
				forM_ (zip [0..7] (ce ^. element . factor)) $ \(p,v) -> writeTChan mco (p,floor (v * 128),False)
				
        forkIO . forever $ do
		y <- atomically $ do
			(n,s) <- readTChan mci
			y <- readTVar tr
			modifyTVar (cs !! y) $ (element . factor %~  
				M.elems . M.adjust (const $ fromIntegral s /128) n . M.fromList . zip [0..7] . (++ repeat 0))
			return y
		(atomically $ readTVar $ cs !! y) >>= print 
	return ()
