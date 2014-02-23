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
        -> IO (TChan (Int,Int))
midiIn name recha = do
	incha <- newTChanIO

  	forkIO $(`catch` \e -> putStrLn $ "midi_exception: " ++ show e)  $ do
		withDefault Block $ \h -> do
			setName (h :: Sound.ALSA.Sequencer.T InputMode) $ name ++ " ctrl_in"
			c <- getId h
			withSimple h "ctrl_in" (caps [capWrite, capSubsWrite]) typeMidiGeneric $ \p -> let 
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
   	return incha		
-- | Loopbroadcast control midi message on a specific channel
midiOut :: String  -- ^ client name
        -> Int     -- ^ broadcast midi channel
        -> IO (TChan (Int,Int,Bool))
midiOut name recha  = do
	ech <- newTChanIO
	forkIO $ (`catch` \e -> putStrLn $ "midi_exception: " ++ show e) $ do
		withDefault Block $ \h -> do
			setName (h :: Sound.ALSA.Sequencer.T OutputMode) $ name ++ " ctrl_out"
			c <- getId h
			withSimple h "ctrl_out" (caps [capRead, capSubsRead]) typeMidiGeneric $ \p -> forever $ do
				(par,val,t) <- atomically $ readTChan ech
				if not t then 	let ev =  forConnection (toSubscribers (Sound.ALSA.Sequencer.Address.Cons c p)) $ 
							CtrlEv Controller (Ctrl 
								(Channel $ fromIntegral recha) 
								(Parameter $ fromIntegral par) 
								(Value $ fromIntegral val)
								)
						in void $ outputDirect h $ ev 
			
				  else  do
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
	return ech
