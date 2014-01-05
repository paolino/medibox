{-# LANGUAGE ViewPatterns, TemplateHaskell #-}

import Control.Concurrent.STM
import Control.Monad
import qualified Data.ByteString as B

import Data.Time.LocalTime
import Prelude 
import Data.List
import Sound.ALSA.Sequencer.Address
import Sound.ALSA.Sequencer.Client
import Sound.ALSA.Sequencer.Port hiding (delete)
import Sound.ALSA.Sequencer.Event
import Sound.ALSA.Sequencer.Connect
import Sound.ALSA.Sequencer hiding (delete)
import Sound.ALSA.Exception  hiding (show)

import Control.DeepSeq
import Control.DeepSeq.TH
import Data.Binary
import qualified Data.Map as M
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Environment

-- | Loop-accept control midi message on a specific channel
midiInDump  :: String  -- ^ client name
        -> IO ()
midiInDump name  = (`catch` \e -> putStrLn $ "midi_exception: " ++ show e)  $ do
  withDefault Block $ \h -> do
        setName (h :: Sound.ALSA.Sequencer.T InputMode) name
        c <- getId h
        withSimple h "midi in" (caps [capWrite, capSubsWrite]) typeMidiGeneric $ \p -> forever $ do
                ev <-  input h
                case body ev of
                     ExtEv SysEx x -> B.putStr x
                     _ -> return ()           

-- | Loop-accept control midi message on a specific channel
midiIn  :: String  -- ^ client name
        -> TChan (Int,Int,Int)  -- ^ event channel
        -> IO ()
midiIn name incha = (`catch` \e -> putStrLn $ "midi_exception: " ++ show e)  $ do
  withDefault Block $ \h -> do
        setName (h :: Sound.ALSA.Sequencer.T InputMode) "md ctrl in"
        c <- getId h
        withSimple h "midi in" (caps [capWrite, capSubsWrite]) typeMidiGeneric $ \p -> forever $ do
                ev <-  input h
                case body ev of
                     CtrlEv Controller (Ctrl 
                                        (Channel (fromIntegral -> cha)) 
                                        (Parameter (fromIntegral -> par)) 
                                        (Value (fromIntegral -> val))
                                        ) -> atomically $ writeTChan incha (cha, par,val)
                     _ -> return ()           
                
-- | Loop-broadcast control midi message on a specific channel
midiOut :: String  -- ^ client name
        -> TChan (Int,Int,Int)  -- ^ event channel
        -> IO ()
midiOut name ech = (`catch` \e -> putStrLn $ "midi_exception: " ++ show e) $ do
  withDefault Block $ \h -> do
        setName (h :: Sound.ALSA.Sequencer.T OutputMode) "md feedback"
        c <- getId h
        withSimple h "midi out" (caps [capRead, capSubsRead]) typeMidiGeneric $ \p -> forever $ do
                (cha,par,val) <- atomically $ readTChan ech
                let ev =  forConnection (toSubscribers (Sound.ALSA.Sequencer.Address.Cons c p)) $ 
                                CtrlEv Controller (Ctrl 
                                        (Channel $ fromIntegral cha) 
                                        (Parameter $ fromIntegral par) 
                                        (Value $ fromIntegral val)
                                        )
                void $ outputDirect h $ ev 

midiOutNote :: String  -- ^ client name
        -> TChan (Int,Int,Int,Bool)  -- ^ event channel
        -> IO ()
midiOutNote name ech = (`catch` \e -> putStrLn $ "midi_exception: " ++ show e) $ do
  withDefault Block $ \h -> do
        setName (h :: Sound.ALSA.Sequencer.T OutputMode) "md note out"
        c <- getId h
        withSimple h "midi note out" (caps [capRead, capSubsRead]) typeMidiGeneric $ \p -> forever $ do
                (cha,pitch,strength,no) <- atomically $ readTChan ech
                let ev =  forConnection (toSubscribers (Sound.ALSA.Sequencer.Address.Cons c p)) $ 
                                NoteEv (if no then NoteOn else NoteOff) (simpleNote
                                        (Channel $ fromIntegral cha) 
                                        (Pitch $ fromIntegral pitch) 
                                        (Velocity $ fromIntegral strength)
                                        )
                void $ outputDirect h $ ev 
{-
midiOutDump :: String  -- ^ client nawme
        -> TChan ()  -- ^ event channel
        -> IO ()
midiOutDump name ech = (`catch` \e -> putStrLn $ "midi_exception: " ++ show e) $ do
  withDefault Block $ \h -> do
        setName (h :: Sound.ALSA.Sequencer.T OutputMode) name
        c <- getId h
        withSimple h "midi dump out" (caps [capRead, capSubsRead]) typeMidiGeneric $ \p -> forever $ do
                (cha,pitch,strength,no) <- atomically $ readTChan ech
                let ev =  forConnection (toSubscribers (Sound.ALSA.Sequencer.Address.Cons c p)) $ 
                                SysEv (if no then NoteOn else NoteOff) (simpleNote
                                        (Channel $ fromIntegral cha) 
                                        (Pitch $ fromIntegral pitch) 
                                        (Velocity $ fromIntegral strength)
                                        )
                void $ outputDirect h $ ev 
-}

-- | Light fork midiIn an midiOut threads
midiInOut  :: String  -- ^ client name
        -> IO (TChan (Int,Int,Int), TChan (Int,Int,Int), TChan (Int,Int,Int,Bool), IO ()) --  ^ communication channels and the kill both threads thread action
midiInOut name  = do
        incha <- newTChanIO
        outcha <- newTChanIO
	outnote <- newTChanIO
        ti <- forkIO $ midiIn name incha
        to <- forkIO $ midiOut name outcha
        ton <- forkIO $ midiOutNote name outnote
	tix <- forkIO $ midiInDump name
        return $ (incha, outcha, outnote, killThread ti >> killThread to >> killThread ton)
        

data Nota = Nota {
	pitch :: Int,
	strength :: Int,
	shift :: Int,
	dur :: Int,
	rep :: Int,
	space :: Int
	} deriving (Show,Read)

type Octus = M.Map Int (M.Map Int (M.Map Int Nota))
 
data Track = Track {
	volume :: Int,
	octus :: Octus
	}	

trackAdjust f = M.adjust (\(Track v o) -> Track v $ f o)
instance Binary Nota where
	put (Nota p s sh d r sp) = put p >> put s >> put sh >> put d >> put r >> put sp
	get = do 
		p <- get
		s <- get
		sh <- get
		d <- get
		r <- get 
		sp <- get 
		return $ Nota p s sh d r sp

$(deriveNFData ''Nota)


instance Binary Track where
	put (Track v o) = put v >> put o
	get = do 
		v <- get
		o <- get 
		return (Track v o)

$(deriveNFData ''Track)

sel 0 (Nota p s sh d r sp) = p
sel 1 (Nota p s sh d r sp) = s
sel 3 (Nota p s sh d r sp) = sh
sel 2 (Nota p s sh d r sp) = d
sel 4 (Nota p s sh d r sp) = r 
sel 5 (Nota p s sh d r sp) = sp



type Attimo = [Nota]



render tv t ms v (c,Nota p s sh d 0 sp) = return ()
render tv t ms v (c,Nota p 0 sh d r sp) = return ()
render tv t ms v (c,n@(Nota p s sh d r sp)) = do
	threadDelay $ (sh+ ms) * floor (120000*8/128)
	cv <- flip (M.!) c `fmap` atomically (readTVar tv)
	let r = d
	forM_ [0 .. r - 1] $ \k -> do 
		forkIO $ do 
			atomically $ writeTChan t (c,p,floor $ fromIntegral (r - k) / fromIntegral (r) * fromIntegral cv * fromIntegral s * fromIntegral v / 128 / 128 ,True)
			threadDelay $ floor (120000*8/fromIntegral d * fromIntegral (127 - sp) / 128)
			atomically $ writeTChan t (c,p,0,False)
		threadDelay $ floor (120000*8/fromIntegral d) 
		

renderA tv t ms v = mapM_ (\(m,x) -> forkIO . render tv t m v $ x) . zip ms


main = do
	ls <- getArgs
	let file = case ls of 
		[] -> "last.mde" 
		(x:_) -> x ++ ".mde"
	rem <- decodeFile file :: IO (M.Map Int Track)
	n <- deepseq () `fmap` newTVarIO rem
	(t1,t2,tn,k) <- midiInOut "paolino"
	-- n <- newTVarIO (M.fromList . zip [0..7] . repeat . Track 0 . M.fromList . zip [0..7] . repeat . M.fromList . zip [0..7] . repeat . M.fromList . zip [0..7] $ repeat (Nota 0 0 0 0 1 1) :: M.Map Int Track) 
	atomically (modifyTVar n $ M.adjust (\(Track v o) -> Track 127 o) 0)
	trit <- newTVarIO (M.fromList . zip [0..7] . repeat $ 0)
	tedit <- newTVarIO []
	ttrack <- newTVarIO 0
	tsound <- newTVarIO 0
	tvolumes <-  newTVarIO (M.fromList . zip [0..7] . repeat $ 127)
	let pla k = do 
		threadDelay $ 120000*8
		forkIO $ do 
			f <- atomically $ do 
				ms <- readTVar trit
				zt <- readTVar n
				return $ \f -> f ms zt
			f $ \ms zt  ->  forM_ [0..7] $ \t -> do
				let Track v z = zt M.! t
				forM_ [0..7] $ 
					\r -> renderA tvolumes tn (M.elems $ ms) v . (M.assocs . flip (M.!) r . flip (M.!) ((k `mod` 8))) $ z
		pla $ k + 1
	w <- forkIO $ pla 0
	forkIO $ forever $ do 
		 (c_,p_,v_) <- atomically $ readTChan t1
		 print (c_,p_,v_);
		 case p_ of
				126 -> do
					t <- zonedTimeToLocalTime `fmap` getZonedTime
					z <- atomically (readTVar n) 
					encodeFile (show t ++ ".mde") $ z
				-- view a piece
				118 -> atomically $ do
					t <- readTVar ttrack
					z <- octus `fmap` flip (M.!) t `fmap` readTVar n	
					c_ <- readTVar tsound
					mapM_ (writeTChan t2) [(0,j*8 + i,sel j $ (((z M.! v_) M.! i) M.! c_)) | j <- [0..5] , i <- [0..7]]
				-- sound delay
				125 -> atomically $ do 
						c_ <- readTVar tsound
						modifyTVar trit . flip M.adjust c_ $ \_ -> v_
				-- change sound
				123 -> atomically $ when (v_ < 8) $ do 
						writeTVar tsound v_
					 	mb <- readTVar tedit
						c_ <- readTVar tsound
						case mb of 
							[] -> return ()
							(bank:_) -> do
								t <- readTVar ttrack
								z <- octus `fmap` flip (M.!) t `fmap` readTVar n	
								mapM_ (writeTChan t2) [(0,j*8 + i,sel j $ (((z M.! bank) M.! i) M.! c_)) | j <- [0..5] , i <- [0..7]]
							
				119 -> atomically $ do 
					writeTVar ttrack v_
					mb <- readTVar tedit
					c_ <- readTVar tsound
					case mb of 
						[] -> return ()
						(bank:_) -> do
							t <- readTVar ttrack
							z <- octus `fmap` flip (M.!) t `fmap` readTVar n	
							mapM_ (writeTChan t2) [(0,j*8 + i,sel j $ (((z M.! bank) M.! i) M.! c_)) | j <- [0..5] , i <- [0..7]]

				-- change value
				_ -> 	if p_ `elem` [104 .. 111] then let p = p_ - 104 in
						atomically $ modifyTVar tvolumes $ flip M.adjust p $ const v_
				    	else if p_ `elem` [96 .. 103] then let p = p_ - 96 in 
						case v_ of 
							0 -> atomically $ do 
								modifyTVar tedit $ delete p
								c_ <- readTVar tsound
								mb <- readTVar tedit
								case mb of 
									[] -> return ()
									(bank:_) -> do
										t <- readTVar ttrack
										z <- octus `fmap` flip (M.!) t `fmap` readTVar n	
										mapM_ (writeTChan t2) [(0,j*8 + i,sel j $ (((z M.! bank) M.! i) M.! c_)) | j <- [0..5] , i <- [0..7]]
							_ -> atomically $ do 
								modifyTVar tedit $ (p :) . filter (/= p)
								c_ <- readTVar tsound
								t <- readTVar ttrack
								z <- octus `fmap` flip (M.!) t `fmap` readTVar n	
								mapM_ (writeTChan t2) [(0,j*8 + i,sel j $ (((z M.! p) M.! i) M.! c_)) | j <- [0..5] , i <- [0..7]]

						

						else atomically $ do 
							mb <- readTVar tedit
							c_ <- readTVar tsound
							t <- readTVar ttrack
							case mb of 
								[] -> return ()
								banks -> forM_ banks $ \bank -> 
									modifyTVar n . flip trackAdjust t .   
											flip M.adjust bank . 
											 flip M.adjust (p_ `mod` 8) . 
											  flip M.adjust c_ $ 
										\(Nota p s sh d r sp) -> case (p_ `div` 8) of 
											0 -> (Nota v_ s sh d r sp)
											1 -> (Nota p v_ sh d r sp)
											2 -> (Nota p s sh v_ r sp)
											3 -> (Nota p s v_ d r sp)
											4 -> (Nota p s sh d v_ sp)
											5 -> (Nota p s sh d r v_)
											_ -> (Nota p s sh d r sp)
							 
	l <- getLine
	killThread w
	threadDelay $ 120000*16
	z <- atomically $ readTVar n
	case l of
		[] -> encodeFile "last.mde" z
		file -> encodeFile (file ++ ".mde") z
		
		 
		
