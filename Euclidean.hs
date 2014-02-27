
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
import Sequencer
import MidiComm

import Control.Lens
import Control.Lens.TH

import Data.Binary
import System.Console.Haskeline
import Data.List.Zipper

seque k ls = case filter (>0) ls of
		[] -> []
		ls -> takeWhile (<1) $ scanl (+) 0 $ drop k $ cycle ls

------------------- Clocked --------------------------

data Dependence = None | Sync | Up Int deriving (Show,Read,Eq)
instance Binary Dependence where
	put None = put 'a'
	put Sync = put 'b'
	put (Up i) = put 'c' >> put i 	
	get = get >>= \l -> case l of
		'a' -> return None
		'b' -> return Sync
		'c' -> Up `fmap` get	 

data Clocked a = Clocked 
	{ 	_clock :: [Dependence]
	,	_element :: a
	}
        deriving (Show)
	
$(makeLenses ''Clocked)


instance Binary a => Binary (Clocked a) where
	put (Clocked x y) = put x >> put y 
	get = do  
                x <- get
		y <- get
		return (Clocked x y)

data ClockedField = Clock [Dependence] deriving (Show,Read)
setClockedField (Clock i)  =  set clock i
------------------------------------------------------

---------------- Euclidean ------------------------------
data Euclidean = Euclidean 
	{	_factor :: [Double]
	,	_offset :: Integer
	,	_group :: Integer
	,	_delay :: Integer
	}
        deriving (Show)	
$(makeLenses ''Euclidean)
instance Binary Euclidean where
	put (Euclidean x1 x2 x3 x4) = put x1 >> put x2 >> put x3 >> put x4  
	get = do  
                x1 <- get
		x2 <- get
		x3 <- get
		x4 <- get
		return (Euclidean x1 x2 x3 x4)

data EuclideanField = Factor [Double] | Offset Integer | Sub Integer | Delay Integer 
        deriving (Show,Read)
setRythmField (Factor i)  =  set  (element . factor ) i
setRythmField (Offset i)  =  set  (element . offset ) i
setRythmField (Sub i)  =  set (element . group ) i
setRythmField (Delay i)  =  set (element . delay ) i

----------------------------------------------------

---------------- Instrument -----------------------
data Instrument = Instrument
	{	_volume	:: Double
	,	_sample :: Integer
	,	_unmuted :: Bool 
	,	_effects :: M.Map Int Double
	} deriving (Show)

$(makeLenses ''Instrument)
instance Binary Instrument where
	put (Instrument x1 x2 x3 x4 ) = put x1 >> put x2 >> put x3 >> put x4 	
	get = do  
                x1 <- get
		x2 <- get
		x3 <- get
		x4 <- get	
		return (Instrument x1 x2 x3 x4)

data InstrumentFieled =  Volume Double | Sample Integer | Unmuted Bool | Effect Int Integer
        deriving (Show,Read)
setInstrField (Volume i)  =  set (element . volume ) i
setInstrField (Sample i)  =  set (element . sample ) i
setInstrField (Unmuted i)  =  set (element . unmuted ) i
setInstrField (Effect i x)  =  over (element . effects ) (M.adjust (const $ fromIntegral x/1000) i)

----------------------------------------------------


data Select = Rythm Int | Instr Int  deriving (Show,Read)


mkConfig :: [Dependence] -> Clocked Euclidean -> (Config Dependence,[Dependence])
mkConfig [] (Clocked [] x) = (Config None [] (0,1),[])
mkConfig [] (Clocked cl x) = mkConfig cl (Clocked cl x)
mkConfig (cl:cls) (Clocked _ (Euclidean fa ofs gr de)) = (Config cl (map (flip (,) gr . floor . (* fromIntegral gr)) $ seque (fromIntegral ofs) fa) (de,gr),cls)



render :: Int ->  Instrument -> (Time,Period,Period) -> IO ()
render c (Instrument v sa m ef) (t,dt,dt') = when m $ playSample c sa (t + 0.2) 0.5 (4*v*dt') dt (ef) 3 1

ntrack = 32


data F = Pattern (Clocked Euclidean) | Sound  (Clocked Intrument)

renderSynth :: Object F -> IO ()
renderSynth (Object is os (Pattern _))  = do
			polygonSmooth $= Enabled
			color (Color4 0.8 0.9 1 0.1:: Color4 GLfloat)
			renderPrimitive Quads $ do
                                vertex (Vertex2 0.1 0 :: Vertex2 GLfloat)
                                vertex (Vertex2 0.9 0 :: Vertex2 GLfloat)
                                vertex (Vertex2 0.9 1 :: Vertex2 GLfloat)
                                vertex (Vertex2 0.1 1 :: Vertex2 GLfloat)
			renderPrimitive Quads $ do
                                vertex (Vertex2 0 0.1 :: Vertex2 GLfloat)
                                vertex (Vertex2 1 0.1 :: Vertex2 GLfloat)
                                vertex (Vertex2 1 0.9 :: Vertex2 GLfloat)
                                vertex (Vertex2 0 0.9 :: Vertex2 GLfloat)
renderSynth (Object is os (Sound _))  = do
			polygonSmooth $= Enabled
			color (Color4 1 0.9 1 0.8:: Color4 GLfloat)
			renderPrimitive Quads $ do
                                vertex (Vertex2 0.1 0 :: Vertex2 GLfloat)
                                vertex (Vertex2 0.9 0 :: Vertex2 GLfloat)
                                vertex (Vertex2 0.9 1 :: Vertex2 GLfloat)
                                vertex (Vertex2 0.1 1 :: Vertex2 GLfloat)
			renderPrimitive Quads $ do
                                vertex (Vertex2 0 0.1 :: Vertex2 GLfloat)
                                vertex (Vertex2 1 0.1 :: Vertex2 GLfloat)
                                vertex (Vertex2 1 0.9 :: Vertex2 GLfloat)
                                vertex (Vertex2 0 0.9 :: Vertex2 GLfloat)

basePattern  = Object 
		(M.singleton 0 (SInput (-0.1,0.5) (0.5,0.5) ["pattern"]))
		(M.singleton 0 (SOutput (1.1,0.5) (0.5,0.5) "pattern" ))
		(Pattern $  (Euclidean [] 0 4 0))
baseSound  = Object 
		(M.singleton 0 (SInput (-0.1,0.5) (0.5,0.5) ["pattern"]))
		M.empty
		(Pattern $  Instrument 0.5 50 True $ M.fromList $ zip [0..] $ replicate 24 0)
	
main = do
        t <- time
	te <- newTChanIO  :: IO (TChan (Event Dependence))
	-- (cs',es') <- decodeFile "current.bb"

	graph <- newTVarIO $ flip insert empty $ Graph (M.fromList $ 
		[ (0,(Affine (0.5,0.5) (0.1,0.06),basePattern)
		, (1,(Affine (0.5,0.5) (0.06,0.1),baseSound ))
		]) M.empty M.empty

		
	-- cs <- forM [0..ntrack - 1] $ \_ -> newTVarIO $ (Clocked [] $ Euclidean [] 0 4 0)
	-- es <- forM [0..ntrack - 1] $ \_ -> newTVarIO (Clocked [] $ Instrument 0.5 50 True $ M.fromList $ zip [0..] $ replicate 24 0)

	-- master tempo (2 sec)
	count <- newTVarIO 0
	forkIO $ 	let l t = do
				atomically $ do 
					writeTChan te (Event Sync t 2)
					modifyTVar count (+1)
				pauseThreadUntil t
				l (t + 2)
		  	in l t

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
	forkIO . forever $ do
		Event j t dt <- atomically $ readTChan te
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
