
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

rythm n m = zipWith subtract `ap` tail $  map floor $ map (/ n) [0,fromIntegral m ..]
seque n m k = takeWhile (\(x,y) -> x < y)  . map (flip (,) m) . scanl (+) 0 . drop (fromIntegral k) $ rythm n m

{-
data Config = Config {
	       colpi :: [Beat] -- unsorted, finite
        ,	late :: Beat
	,	window :: Event
	}

data Event = Event {
        when :: Time
        , dur :: Period
        } deriving Show
-}


data Environment = Environment
	{	_clock :: Integer
	,	_factor :: Double
	,	_offset :: Integer
	,	_group :: Integer
	,	_volume	:: Double
	,	_delay :: Integer
	,	_sample :: Integer
	,	_unmuted :: Bool 
	,	_effects :: M.Map Int Double
	} deriving (Show)

$(makeLenses ''Environment)
data Field = Clock Integer | Factor Double | Offset Integer | Sub Integer | Volume Double | Delay Integer | Sample Integer | Unmuted Bool | Effect Int Integer
        deriving (Show,Read)
setField :: Field -> Environment -> Environment
setField (Clock i)  =  set clock i
setField (Factor i)  =  set  factor i
setField (Offset i)  =  set  offset i
setField (Sub i)  =  set group i
setField (Volume i)  =  set volume i
setField (Delay i)  =  set delay i
setField (Sample i)  =  set sample i
setField (Unmuted i)  =  set unmuted i
setField (Effect i x)  =  over effects (M.adjust (const $ fromIntegral x/1000) i)

data Query = Query Int deriving (Show,Read)

mkConfig :: Environment -> Config
mkConfig (Environment cl fa ofs gr _ de _ _ _) = Config cl (seque (1 + fa * (fromIntegral gr - 1)) gr $ fromIntegral ofs) (de,gr)


render :: Int ->  Environment -> (Time,Period,Period) -> IO ()
render c (Environment _ _ _ _ v de sa m ef) (t,dt,dt') = when m $ playSample c sa (t + 0.2) 0.5 (4*v*dt') dt (ef) 3 1

main = do
        t <- time
	te <- newTChanIO 
        c1 <- newTVarIO $ (Environment 0 0 0 4 0.5 0 50 True $ M.fromList $ zip [0..] $ replicate 24 0)
        c2 <- newTVarIO $ (Environment 1 0 0 4 0.5 0 50 True $ M.fromList $ zip [0..] $ replicate 24 0)
        c3 <- newTVarIO $ (Environment 2 0 0 4 0.5 0 50 True $ M.fromList $ zip [0..] $ replicate 24 0)
        c4 <- newTVarIO $ (Environment 3 0 0 4 0.5 0 50 True $ M.fromList $ zip [0..] $ replicate 24 0)
        c5 <- newTVarIO $ (Environment 4 0 0 4 0.5 0 50 True $ M.fromList $ zip [0..] $ replicate 24 0)
        c6 <- newTVarIO $ (Environment 5 0 0 4 0.5 0 50 True $ M.fromList $ zip [0..] $ replicate 24 0)
        c7 <- newTVarIO $ (Environment 6 0 0 4 0.5 0 50 True $ M.fromList $ zip [0..] $ replicate 24 0)
        c8 <- newTVarIO $ (Environment 7 0 0 4 0.5 0 50 True $ M.fromList $ zip [0..] $ replicate 24 0)
	let cs = [c1,c2,c3,c4,c5,c6,c7,c8]
	forM_ (zip [1..] cs) $ \(n,c) -> do
		te' <- atomically $ dupTChan te
		forkIO $ sequencer  n te' (mkConfig `fmap` readTVar c)
	forkIO $ 	let l t = do
				atomically $ writeTChan te (Event 0 t 2)
				pauseThreadUntil t
				l (t + 2)
		  	in l t

	mj <- newTVarIO (M.fromList $ zip [1..8] $ repeat 0)
	forkIO . forever $ do
		Event j t dt <- atomically $ readTChan te
                dt' <- flip (M.!) j `fmap` atomically (readTVar mj)
                atomically $ modifyTVar mj $ M.adjust (const dt) j
		when (j > 0) $ do 
			e <- atomically (readTVar $ cs !! (fromIntegral j - 1))
			render (fromIntegral j - 1) e (t,dt,dt')
	midi cs
        tk <- newTVarIO 0
	let input = do
                l <- getLine
                case l of
                        "end" -> return ()
                        _ -> case reads l of
                                [(i,_)] -> do 
                                        atomically $ writeTVar tk $ i `mod` 8
                                        input 
                                        
                                _ -> case reads l of
                                        [(c,_)] -> do 
                                                atomically $ do 
                                                        i <- readTVar tk        
                                                        modifyTVar (cs !! i) $ setField c
                                                input
                
                                        _ -> case reads l of
                                                [(Query j,_)] -> do 
                                                        v <- atomically $ readTVar (cs !! j)
                                                        print v
                                                        input
                                                _ ->  print "lost" >> input
        input
 
{-

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

main = do	
  	ws <- decodeFile "current.bb"
	se <- forM ws $ newTVarIO
	-- se <- replicateM 8 $ newTVarIO (Environment (RaffineParams 7 0 12 $ rythm 7 12) 0 4 0 0 False $ M.fromList $ zip [0..] $ replicate 24 0)
	tap <- newTVarIO (3,1)
	let 	loop :: Int  -> TVar Environment -> Sequencer Raffine1 -> IO ()
		loop c s (Sequencer f) = f (affineseq c tap s) >>= loop c s
	forM_ (zip [0 ..] se) $ \(c,s) -> forkIO $ loop c s $  sequencer affinestep (Feedback (RaffineParams 7 0 12 $ rythm 7 12) (rythm 7 12) (0,1) 1)
	getLine
	ws <- atomically $ mapM readTVar se
		
 	encodeFile "current.bb" ws

	


-}

midi se = 	do
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
		e@(Environment d fa ofs gr v de sa m ef) <- readTVar (se !! y)
		writeTChan tfbp2 (24, floor $ v * 128,False)
		writeTChan tfbp2 (31, fromIntegral sa,True )
		writeTChan tfbp2 (25, fromIntegral de, False)
		writeTChan tfbp2 (26, fromIntegral $ ofs,False)
		writeTChan tfbp2 (27, fromIntegral $ floor (fa*128),False)
		writeTChan tfbp2 (28, fromIntegral $ gr,False)
		writeTChan tfbp2 (29, fromIntegral $ d,False)

	inst <- newTVarIO 0
	forkIO . forever . atomically $ do
				(n,s) <- readTChan mc
				y <- readTVar inst
				case n of 
					127 -> do 
						writeTVar inst $ fromIntegral s
						writeTChan fbinst $ fromIntegral s	
					24 -> modifyTVar (se !! y) $ volume .~ fromIntegral s/128
					29 -> modifyTVar (se !! y) $ clock .~ (if s <= y then fromIntegral s else 0) 
					31 -> modifyTVar (se !! y) $ sample .~ fromIntegral s
					25 -> modifyTVar (se !! y) $ delay .~ fromIntegral s
					26 -> modifyTVar (se !! y) $ offset .~ fromIntegral s
					27 -> modifyTVar (se !! y) $ factor .~ (fromIntegral s/128)
					28 -> modifyTVar (se !! y) $ group .~ (fromIntegral s)
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

