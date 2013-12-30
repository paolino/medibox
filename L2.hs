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
midiIn  :: String  -- ^ client name
        -> TChan (Int,Int,Int)  -- ^ event channel
        -> IO ()
midiIn name incha = (`catch` \e -> putStrLn $ "midi_exception: " ++ show e)  $ do
  withDefault Block $ \h -> do
        setName (h :: Sound.ALSA.Sequencer.T InputMode) name
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
        setName (h :: Sound.ALSA.Sequencer.T OutputMode) name
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

-- | Light fork midiIn an midiOut threads
midiInOut  :: String  -- ^ client name
        -> IO (TChan (Int,Int,Int), TChan (Int,Int,Int), IO ()) --  ^ communication channels and the kill both threads thread action
midiInOut name  = do
        incha <- newTChanIO
        outcha <- newTChanIO
        ti <- forkIO $ midiIn name incha
        to <- forkIO $ midiOut name outcha
        return $ (incha, outcha,  killThread ti >> killThread to )
        



main = do
	(t1,t2,k) <- midiInOut "midi set"
	forever $ do 
		r <- getLine
		let cha:par:val:_ = map read $ words r
		print (cha,par,val)
		atomically $ writeTChan t2 (cha,par,val)
						 
					
		 
		
