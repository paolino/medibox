{-# LANGUAGE ViewPatterns #-}
module MidiComm where


import Prelude hiding (show)

import Sound.ALSA.Sequencer.Address
import Sound.ALSA.Sequencer.Client
import Sound.ALSA.Sequencer.Port
import Sound.ALSA.Sequencer.Event
import Sound.ALSA.Sequencer.Connect
import Sound.ALSA.Sequencer 
import Sound.ALSA.Exception  


import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

midiInNoteOn :: String  -- ^ client name
        -> TChan (Int,Int,Int)  -- ^ event channel
        -> IO ()
midiInNoteOn name incha = (`catch` \e -> putStrLn $ "midi_exception: " ++ show e) $ do
  withDefault Block $ \h -> do
        setName (h :: Sound.ALSA.Sequencer.T InputMode) $ name ++ "note out"
        c <- getId h
        withSimple h "midi in" (caps [capWrite, capSubsWrite]) typeMidiGeneric $ \p -> forever $ do
                ev <-  input h
                case body ev of
                     NoteEv NoteOn      (Note  
                                        (Channel (fromIntegral -> cha)) 
                                        (Pitch (fromIntegral -> pitch)) 
                                        (Velocity (fromIntegral -> vel))
                                        _
                                        _
                                        ) -> atomically $ writeTChan incha (cha,pitch,vel)
                     _ -> return ()           

midiOutNote :: String  -- ^ client name
        -> TChan (Int,Int,Int,Bool)  -- ^ event channel
        -> IO ()
midiOutNote name ech = (`catch` \e -> putStrLn $ "midi_exception: " ++ show e) $ do
  tn <- newTVarIO 0
  withDefault Block $ \h -> do
        setName (h :: Sound.ALSA.Sequencer.T OutputMode) $ name ++ "note out"
        c <- getId h
        withSimple h "midi out" (caps [capRead, capSubsRead]) typeMidiGeneric $ \p -> forever $ do
                (cha,pitch,strength,no) <- atomically $ readTChan ech
                let ev =  forConnection (toSubscribers (Sound.ALSA.Sequencer.Address.Cons c p)) $ 
                                NoteEv (if no then NoteOn else NoteOff) (simpleNote
                                        (Channel $ fromIntegral cha) 
                                        (Pitch $ fromIntegral pitch) 
                                        (Velocity $ fromIntegral strength)
                                        )
                void $ outputDirect h $ ev


data Parser = NonReg99 Int | NonReg98 Int Int | NonReg6 Int Int Int | Zot [(Int,Int)]


nrparse :: Parser -> (Int,Int) -> Parser
nrparse (Zot _) (x,y) 
	| x == 99 = NonReg99 y
	| otherwise = Zot [(x,y)]
nrparse (NonReg99 i) (x,y) 
	| x == 98 = NonReg98 i y
	| otherwise = Zot [(99,i),(x,y)]
nrparse (NonReg98 i j) (x,y)
	| x == 6 = NonReg6 i j y
	| otherwise = Zot [(99,i),(98,j), (x,y)] 
nrparse (NonReg6 i j f) (x,y)
	| x == 38 = Zot [(i*127 + j, f*127 + y)]
	| otherwise = Zot [(99,i),(98,j),(38,f),(x,y)] 

-- | Loop-accept control midi message on a specific channel
midiIn  :: String  -- ^ client name
        -> Int     -- ^ listening midi channel
        -> TChan (Int,Int)  -- ^ event channel
        -> IO ()
midiIn name recha incha = (`catch` \e -> putStrLn $ "midi_exception: " ++ show e)  $ do
  withDefault Block $ \h -> do
        setName (h :: Sound.ALSA.Sequencer.T InputMode) $ name ++ "ctrl in"
        c <- getId h
        withSimple h "midi in" (caps [capWrite, capSubsWrite]) typeMidiGeneric $ \p -> let 
		loop s =  do
			ev <-  input h
			case body ev of
			     CtrlEv Controller (Ctrl 
						(Channel (fromIntegral -> cha)) 
						(Parameter (fromIntegral -> par)) 
						(Value (fromIntegral -> val))
						) -> do 
							s' <- 	if cha == recha then do
									let s' = nrparse s (par,val)
									case s' of 
										Zot evs -> atomically . mapM_ (writeTChan incha) $ evs
										_ -> return ()
									return s'
								else return s
							loop s'
								
							
			     _ -> loop s
		in loop $ Zot []
			
-- | Loop-accept control midi message on a specific channel
midiPCIn  :: String  -- ^ client name
        -> Int     -- ^ listening midi channel
        -> TChan (Int,Int)  -- ^ event channel
        -> IO ()
midiPCIn name recha incha = (`catch` \e -> putStrLn $ "midi_exception: " ++ show e)  $ do
  withDefault Block $ \h -> do
        setName (h :: Sound.ALSA.Sequencer.T InputMode) $ name ++ "ctrl in"
        c <- getId h
        withSimple h "midi in" (caps [capWrite, capSubsWrite]) typeMidiGeneric $ \p -> forever $ do
                ev <-  input h
                case body ev of
                     CtrlEv PgmChange (Ctrl 
                                        (Channel (fromIntegral -> cha)) 
                                        (Parameter (fromIntegral -> par)) 
                                        (Value (fromIntegral -> val))
                                        ) -> when  (cha == recha) $ atomically $ writeTChan incha (par,val)
                     _ -> return ()                  
-- | Loop-broadcast control midi message on a specific channel
midiOut :: String  -- ^ client name
        -> Int     -- ^ broadcast midi channel
        -> TChan (Int,Int)  -- ^ event channel
        -> IO ()
midiOut name recha ech = (`catch` \e -> putStrLn $ "midi_exception: " ++ show e) $ do
  withDefault Block $ \h -> do
        setName (h :: Sound.ALSA.Sequencer.T OutputMode) $ name ++ "ctrl out"
        c <- getId h
        withSimple h "midi out" (caps [capRead, capSubsRead]) typeMidiGeneric $ \p -> forever $ do
                (par,val) <- atomically $ readTChan ech
                let ev =  forConnection (toSubscribers (Sound.ALSA.Sequencer.Address.Cons c p)) $ 
                                CtrlEv Controller (Ctrl 
                                        (Channel $ fromIntegral recha) 
                                        (Parameter $ fromIntegral par) 
                                        (Value $ fromIntegral val)
                                        )
                void $ outputDirect h $ ev 
-- | Loop-broadcast control midi message on a specific channel
midiOutNP :: String  -- ^ client name
        -> Int     -- ^ broadcast midi channel
        -> TChan (Int,Int)  -- ^ event channel
        -> IO ()
midiOutNP name recha ech = (`catch` \e -> putStrLn $ "midi_exception: " ++ show e) $ do
  withDefault Block $ \h -> do
        setName (h :: Sound.ALSA.Sequencer.T OutputMode) $ name ++ "ctrl out"
        c <- getId h
        withSimple h "midi out" (caps [capRead, capSubsRead]) typeMidiGeneric $ \p -> forever $ do
                (par,val) <- atomically $ readTChan ech
		let 	(p1,p2) = par `divMod` 127
			(v1,v2) = val `divMod` 127
		forM_ (zip [99,98,6,38] [p1,p2,v1,v2]) $ \(par,val) -> do
			let ev =  forConnection (toSubscribers (Sound.ALSA.Sequencer.Address.Cons c p)) $ 
					CtrlEv Controller (Ctrl 
						(Channel $ fromIntegral recha) 
						(Parameter $ fromIntegral par) 
						(Value $ fromIntegral val)
						)
                	void $ outputDirect h $ ev

-- | Light fork midiIn an midiOut threads
midiInOut  :: String  -- ^ client name
        -> Int     -- ^ broadcast and listening midi channel
        -> IO (TChan (Int,Int), TChan (Int,Int), IO ()) --  ^ communication channels and the kill both threads thread action
midiInOut name recha  = do
        incha <- newTChanIO
        outcha <- newTChanIO
        ti <- forkIO $ midiIn name recha incha
        to <- forkIO $ midiOut name recha outcha
        return $ (incha, outcha, killThread ti >> killThread to)
        
-- | Loop-accept control midi message on a specific channel
midiInNP  :: String  -- ^ client name
        -> Int     -- ^ listening midi channel
        -> TChan (Int,Int)  -- ^ event channel
        -> IO ()
midiInNP name recha incha = (`catch` \e -> putStrLn $ "midi_exception: " ++ show e)  $ do
  withDefault Block $ \h -> do
        setName (h :: Sound.ALSA.Sequencer.T InputMode) $ name ++ "ctrl in"
        c <- getId h
        withSimple h "midi in" (caps [capWrite, capSubsWrite]) typeMidiGeneric $ \p -> forever $ do
                ev <-  input h
                case body ev of
                     CtrlEv NonRegParam (Ctrl 
                                        (Channel (fromIntegral -> cha)) 
                                        (Parameter (fromIntegral -> par)) 
                                        (Value (fromIntegral -> val))
                                        ) -> when  (cha == recha) $ atomically $ writeTChan incha (par,val)
                     _ -> return ()           
{-
main = do
	nc <- newTChanIO >>= \nc -> forkIO (midiInNP "op" 1 nc) >> return nc
	forkIO . forever $ atomically (readTChan nc) >>= print
	nc <- newTChanIO >>= \nc -> forkIO (midiIn "op" 1 nc) >> return nc
	forever $ atomically (readTChan nc) >>= print
-}
