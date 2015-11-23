
{-# LANGUAGE TypeFamilies, TemplateHaskell, Rank2Types #-}

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

data Clocked a = Clocked 
	{ 	_clock :: Integer
	,	_element :: a
	}
        deriving (Show)	
$(makeLenses ''Clocked)
data Euclidean = Euclidean 
	{	_factor :: Double
	,	_offset :: Integer
	,	_group :: Integer
	,	_delay :: Integer
	
	}
        deriving (Show)	
$(makeLenses ''Euclidean)
data Instrument = Instrument
	{	_volume	:: Double
	,	_sample :: Integer
	,	_unmuted :: Bool 
	,	_effects :: M.Map Int Double
	} deriving (Show)

$(makeLenses ''Instrument)

data RythmDeformerField = Index Int | Changes [[EuclideanField]] deriving (Show,Read)
data ClockedField = Clock Integer deriving (Show,Read)
data EuclideanField = Factor Double | Offset Integer | Sub Integer | Delay Integer
        deriving (Show,Read)
data InstrumentFieled =  Volume Double | Sample Integer | Unmuted Bool | Effect Int Integer
        deriving (Show,Read)

data RythmDeformer = RythmDeformer
        {       _rythmindex :: Int
        ,       _efis :: [[EuclideanField]]
        }      deriving (Show)  
$(makeLenses ''RythmDeformer)


setRythmDeformerField (Index i) = set (element . rythmindex) i
setRythmDeformerField (Changes ls) = set (element . efis) ls

setInstrField (Volume i)  =  set (element . volume ) i
setInstrField (Sample i)  =  set (element . sample ) i
setInstrField (Unmuted i)  =  set (element . unmuted ) i
setInstrField (Effect i x)  =  over (element . effects ) (M.adjust (const $ fromIntegral x/1000) i)
setClockedField (Clock i)  =  set clock i

setRythmField (Factor i)  =  set  (element . factor ) i
setRythmField (Offset i)  =  set  (element . offset ) i
setRythmField (Sub i)  =  set (element . group ) i
setRythmField (Delay i)  =  set (element . delay ) i


data Select = Rythm Int | Instr Int | Rdefo Int deriving (Show,Read)


mkConfig :: Clocked Euclidean -> Config
mkConfig (Clocked cl (Euclidean fa ofs gr de)) = Config cl (seque (1 + fa * (fromIntegral gr - 1)) gr $ fromIntegral ofs) (de,gr)


render :: Int ->  Instrument -> (Time,Period,Period) -> IO ()
render c (Instrument v sa m ef) (t,dt,dt') = when m $ playSample c sa (t + 0.2) 0.5 (4*v*dt') dt (ef) 3 1

main = do
        t <- time
	te <- newTChanIO 

	cs <- forM [0..7] $ \_ -> newTVarIO $ (Clocked 0 $ Euclidean 0 0 4 0)
	es <- forM [0..7] $ \_ -> newTVarIO (Clocked 0 $ Instrument 0.5 50 True $ M.fromList $ zip [0..] $ replicate 24 0)
	ds <- forM [0..7] $ \_ -> newTVarIO $ (Clocked 0 $ RythmDeformer 0 $ [])

	-- master tempo (2 sec)
	forkIO $ 	let l t = do
				atomically $ writeTChan te (Event 0 t 2)
				pauseThreadUntil t
				l (t + 2)
		  	in l t

	-- euclidean replicators
	forM_ (zip [1..] cs) $ \(n,c) -> do
		te' <- atomically $ dupTChan te
		forkIO $ sequencer  n te' (mkConfig `fmap` readTVar c)

        -- players
	mj <- newTVarIO (M.fromList $ zip [0..7] $ repeat 0) -- remember last dt
	forkIO . forever $ do
		Event j t dt <- atomically $ readTChan te
		forM_ (zip [0..] es) $ \(i,te) -> do
                                e <- atomically $ readTVar te
                                when (e ^. clock == j) $ do
                                        dt' <- flip (M.!) i `fmap` atomically (readTVar mj)
                                        atomically $ modifyTVar mj $ M.adjust (const dt) i
                                        render (fromIntegral i) (e ^. element) (t,dt,dt')
        
        -- rythm deformers 
        te' <- atomically $ dupTChan te
	mj <- newTVarIO (M.fromList $ zip [0..7] $ repeat []) -- remember last dt
        forkIO . forever $ do
		Event j t dt <- atomically $ readTChan te'
                -- reset each Event on 0
                when (j == 0) $ atomically $ do 
                                ls <- forM ds $ fmap (view (element . efis)) . readTVar 
                                writeTVar mj (M.fromList $ zip [0..7] $ ls)
                -- execute deformers on j
		forM_ (zip [0..] ds) $ \(i,td) -> do 
                                r <- atomically $ do
                                        e <- readTVar td -- clocked deformer
                                        if e ^. clock == j then do
                                                rs <- flip (M.!) i `fmap` readTVar mj -- actual state for deformer i
                                                case rs of
                                                        [] -> return (return ()) -- nothing to deform
                                                        (r : rs) -> do 
                                                                modifyTVar mj $ M.adjust (\_ -> rs) i -- push new state
                                                                -- return deforming action
                                                                return $ do 
                                                                        pauseThreadUntil (t - 0.02) -- wait until just
                                                                        -- deform the pointed euclidean
                                                                        atomically $ modifyTVar (cs !! (e ^. (element .rythmindex))) $ flip (foldr setRythmField) r
                                                else return (return ())
                                        
                                -- fork the result
                                forkIO r

                                                

	ti <- newTVarIO $ 0
	tr <- newTVarIO $ 0
	trd <- newTVarIO $ 0
	tl <- newTVarIO $ Rythm 0
	let input = do
                l <- getLine
                case l of
                        "end" -> return ()
                        _ -> case reads l of
                                [(Rythm i,_)] -> do 
                                        atomically $ writeTVar tr $  i `mod` 8
					atomically $ writeTVar tl (Rythm $ i `mod` 8)
					(atomically $ readTVar $ cs !! i) >>= print 
                                        input 
                                [(Instr i,_)] -> do 
                                        atomically $ writeTVar ti $  i `mod` 8
					atomically $ writeTVar tl (Instr $ i `mod` 8)
					(atomically $ readTVar $ es !! i) >>= print 
                                        input 
                                [(Rdefo i,_)] -> do 
                                        atomically $ writeTVar trd $  i `mod` 8
					atomically $ writeTVar tl (Rdefo $ i `mod` 8)
					(atomically $ readTVar $ ds !! i) >>= print 
                                        input 

				_ -> case reads l of
					[(c ,_)] -> do 
						atomically $ do 
							i <- readTVar tl
                                                        case i of 
								Rythm i -> modifyTVar (cs !! i) $ setClockedField c
								Instr i -> modifyTVar (es !! i) $ setClockedField c
								Rdefo i -> modifyTVar (ds !! i) $ setClockedField c

						input
					_ -> case reads l of
						[(c,_)] -> do 
							atomically $ do 
								i <- readTVar tr      
								modifyTVar (cs !! i) $ setRythmField c
							input
						_ -> case reads l of
							[(c,_)] -> do 
								atomically $ do 
									i <- readTVar ti
									modifyTVar (es !! i) $ setInstrField c
								input
                                                        _ -> case reads l of
                                                                [(c,_)] -> do 
                                                                        atomically $ do 
                                                                                i <- readTVar trd
                                                                                modifyTVar (ds !! i) $ setRythmDeformerField c
                                                                        input

                                                                _ ->  putStrLn "<< parsing failed >>" >> input
        input
 
{-

instance Binary Instrument where
	put (Instrument sh o p co x m y) = put sh >> put o >> put p >> put co >> put x >> put m >>put y
	get = do  
                sh <- get
		o <- get
		p <- get
		co <- get
		x <- get
		m <- get
		y <- get
	
		return (Instrument sh o p co x m y)

main = do	
  	ws <- decodeFile "current.bb"
	se <- forM ws $ newTVarIO
	-- se <- replicateM 8 $ newTVarIO (Instrument (RaffineParams 7 0 12 $ rythm 7 12) 0 4 0 0 False $ M.fromList $ zip [0..] $ replicate 24 0)
	tap <- newTVarIO (3,1)
	let 	loop :: Int  -> TVar Instrument -> Sequencer Raffine1 -> IO ()
		loop c s (Sequencer f) = f (affineseq c tap s) >>= loop c s
	forM_ (zip [0 ..] se) $ \(c,s) -> forkIO $ loop c s $  sequencer affinestep (Feedback (RaffineParams 7 0 12 $ rythm 7 12) (rythm 7 12) (0,1) 1)
	getLine
	ws <- atomically $ mapM readTVar se
		
 	encodeFile "current.bb" ws

	



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
		e@(Instrument d fa ofs gr v de sa m ef) <- readTVar (se !! y)
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
-}

