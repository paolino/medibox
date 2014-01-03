{-# LANGUAGE ViewPatterns #-}

import Control.Concurrent.STM
import Control.Monad
import qualified Data.ByteString as B

import Data.Time.LocalTime
import Prelude 

import Sound.ALSA.Sequencer.Address
import Sound.ALSA.Sequencer.Client
import Sound.ALSA.Sequencer.Port
import Sound.ALSA.Sequencer.Event
import Sound.ALSA.Sequencer.Connect
import Sound.ALSA.Sequencer 
import Sound.ALSA.Exception  hiding (show)


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
	dur :: Int
	} deriving (Show,Read)

sel 0 (Nota p s sh d) = p
sel 1 (Nota p s sh d) = s
sel 3 (Nota p s sh d) = sh
sel 2 (Nota p s sh d) = d

type Attimo = [Nota]



render t ms (c,Nota p 0 sh 0) = return ()
render t ms (c,Nota p 0 sh d) = return ()
render t ms (c,Nota p s sh d) = do
	threadDelay $ (sh+ ms) * floor (120000*8/128)
	atomically $ writeTChan t (c,p,s,True)
	threadDelay $ d* floor (120000*8/128)

	atomically $ writeTChan t (c,p,0,False)

renderA t ms = mapM_ (\(m,x) -> forkIO . render t m $ x) . zip ms


main = do

	rem <- readFile "1.md"
	n <- newTVarIO (read rem)
	(t1,t2,tn,k) <- midiInOut "paolino"
	-- n <- newTVarIO (M.fromList . zip [0..127] . repeat $ M.fromList . zip [0..7] . repeat $ M.fromList . map (\i -> (i,Nota 0 0 0 0)) $ [0..127])
	trit <- newTVarIO (M.fromList . zip [0..7] . repeat $ 0)
	tbank <- newTVarIO 0
	tedit <- newTVarIO 0
	tsound <- newTVarIO 0
	tloop <- newTVarIO 1
	let pla k = do 
		threadDelay $ 120000*8
		print "h"
		forkIO (pla $ k + 1)
		f <- atomically $ do 
			bank <- readTVar tbank
			ms <- readTVar trit
			l <- readTVar tloop
			z <- readTVar n
			return $ \f -> f bank ms z l
		f $ \bank ms z l ->  forM_ [0..7] $ \r -> renderA tn (M.elems $ ms) . (M.assocs . flip (M.!) r . flip (M.!) (bank + (k `mod` l))) $ z
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
				125 -> atomically $ do 
						c_ <- readTVar tsound
						modifyTVar trit . flip M.adjust c_ $ \_ -> v_
				-- set play bank
				124 -> atomically $ do 
						writeTVar tbank v_
				-- set edit bank
				120 -> atomically $ do 
						writeTVar tedit v_
						c_ <- readTVar tsound
						z <- readTVar n	
						mapM_ (writeTChan t2) [(0,j*8 + i,sel j $ (((z M.! v_) M.! i) M.! c_)) | j <- [0..3] , i <- [0..7]]
				-- set edit sound
				123 -> atomically $ do 
						writeTVar tsound v_
						bank <- readTVar tedit
						z <- readTVar n	
						mapM_ (writeTChan t2) [(0,j*8 + i,sel j $ (((z M.! bank) M.! i) M.! v_)) | j <- [0..3] , i <- [0..7]]
				-- copy bank from edit
				122 -> atomically $ do 
						bank <- readTVar tedit
						c_ <- readTVar tsound
						b <- (flip (M.!) bank) `fmap` readTVar n 
						forM_ [0..7] $ \p -> modifyTVar n . flip M.adjust v_ . flip M.adjust p . flip M.adjust c_ $ \_ -> 
							b M.! p M.! c_
				-- set loop length
				121 -> atomically $ do 
						writeTVar tloop v_
				-- change value
				_ -> atomically $ do 
					bank <- readTVar tedit
					c_ <- readTVar tsound
					modifyTVar n . flip M.adjust bank . flip M.adjust (p_ `mod` 8) . flip M.adjust c_ $ 
						\(Nota p s sh d) -> case (p_ `div` 8) of 
							0 -> (Nota v_ s sh d)
							1 -> (Nota p v_ sh d)
							2 -> (Nota p s sh v_)
							3 -> (Nota p s v_ d)
							_ -> (Nota p s sh d)
						 
	l <- getLine
	z <- atomically $ readTVar n
	writeFile "1.md" (show z)		
		 
		
