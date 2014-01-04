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
	rep :: Int
	} deriving (Show,Read)

instance Binary Nota where
	put (Nota p s sh d r) = put p >> put s >> put sh >> put d >> put r
	get = do 
		p <- get
		s <- get
		sh <- get
		d <- get
		r <- get 
		return $ Nota p s sh d r

$(deriveNFData ''Nota)
sel 0 (Nota p s sh d r) = p
sel 1 (Nota p s sh d r) = s
sel 3 (Nota p s sh d r) = sh
sel 2 (Nota p s sh d r) = d
sel 4 (Nota p s sh d r) = r 

type Attimo = [Nota]



render t ms (c,Nota p 0 sh d r) = return ()
render t ms (c,Nota p s sh d r) = do
	threadDelay $ (sh+ ms) * floor (120000*8/128)
	forM_ [1..floor (0.5 + 128/ fromIntegral (d + 1))] $ \_ -> do 
		atomically $ writeTChan t (c,p,s,True)
		threadDelay $ (d + 1) * floor (0.5 + 120000*8/128)
		atomically $ writeTChan t (c,p,0,False)

renderA t ms = mapM_ (\(m,x) -> forkIO . render t m $ x) . zip ms


main = do
	rem <- decodeFile "1.mde"
	n <- newTVarIO rem
	(t1,t2,tn,k) <- midiInOut "paolino"
	-- n <- newTVarIO (M.fromList . zip [0..127] . repeat $ M.fromList . zip [0..7] . repeat $ M.fromList . map (\i -> (i,Nota 0 0 0 0 1)) $ [0..127])
	
	trit <- newTVarIO (M.fromList . zip [0..127] . repeat $ 0)
	tedit <- newTVarIO []
	tsound <- newTVarIO 0
	let pla k = do 
		threadDelay $ 120000*8
		forkIO (pla $ k + 1)
		f <- atomically $ do 
			ms <- readTVar trit
			z <- readTVar n
			return $ \f -> f ms z 
		f $ \ms z  ->  forM_ [0..7] $ \r -> renderA tn (M.elems $ ms) . (M.assocs . flip (M.!) r . flip (M.!) ((k `mod` 8))) $ z
	forkIO $ pla 0
	forkIO $ forever $ do 
		 (c_,p_,v_) <- atomically $ readTChan t1
		 print (c_,p_,v_);
		 case p_ of
				126 -> do
					t <- zonedTimeToLocalTime `fmap` getZonedTime
					z <- atomically (readTVar n) 
					writeFile (show t) . show $ z
				-- sound delay
				118 -> atomically $ do
					z <- readTVar n	
					c_ <- readTVar tsound
					mapM_ (writeTChan t2) [(0,j*8 + i,sel j $ (((z M.! v_) M.! i) M.! c_)) | j <- [0..4] , i <- [0..7]]

				125 -> atomically $ do 
						c_ <- readTVar tsound
						modifyTVar trit . flip M.adjust c_ $ \_ -> v_
				123 -> atomically $ do 
						writeTVar tsound v_
					 	mb <- readTVar tedit
						c_ <- readTVar tsound
						case mb of 
							[] -> return ()
							(bank:_) -> do
								z <- readTVar n	
								mapM_ (writeTChan t2) [(0,j*8 + i,sel j $ (((z M.! bank) M.! i) M.! c_)) | j <- [0..4] , i <- [0..7]]
				-- change value
				_ -> if p_ `elem` [110 .. 117] then let p = p_ - 110 in 
					case v_ of 
						0 -> atomically $ do 
							modifyTVar tedit $ delete p
							c_ <- readTVar tsound
							mb <- readTVar tedit
							case mb of 
								[] -> return ()
								(bank:_) -> do
									z <- readTVar n	
									mapM_ (writeTChan t2) [(0,j*8 + i,sel j $ (((z M.! bank) M.! i) M.! c_)) | j <- [0..4] , i <- [0..7]]
						_ -> atomically $ do 
							modifyTVar tedit $ (p :)
							c_ <- readTVar tsound
							z <- readTVar n	
							mapM_ (writeTChan t2) [(0,j*8 + i,sel j $ (((z M.! p) M.! i) M.! c_)) | j <- [0..4] , i <- [0..7]]

					

					else atomically $ do 
						mb <- readTVar tedit
						c_ <- readTVar tsound
						case mb of 
							[] -> return ()
							banks -> forM_ banks $ \bank -> 
								modifyTVar n . flip M.adjust bank . flip M.adjust (p_ `mod` 8) . flip M.adjust c_ $ 
									\(Nota p s sh d r) -> case (p_ `div` 8) of 
										0 -> (Nota v_ s sh d r)
										1 -> (Nota p v_ sh d r)
										2 -> (Nota p s sh v_ r)
										3 -> (Nota p s v_ d r)
										4 -> (Nota p s sh d v_)
										_ -> (Nota p s sh d r)
						 
	l <- getLine
	z <- atomically $ readTVar n
	encodeFile "1.mde" z
		 
		
