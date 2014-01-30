

{-# LANGUAGE TypeFamilies, FlexibleInstances, NoMonomorphismRestriction #-}

module Samples where

import Control.Monad (forM_,when)

import Sound.OSC
import Sound.SC3 hiding (pitch)
import System.FilePath
import System.FilePath.Glob
import System.Directory



withSC3n :: Int -> Connection UDP a -> IO a
withSC3n i = withTransport (openUDP "127.0.0.1" $ fromIntegral i)

servers = [57110]

bootSample :: Int -> (Int,FilePath) -> IO ()
bootSample j (n,fp) = do
        withSC3n j . send $ b_free n
        withSC3n j . send $ b_allocRead n fp 0 0
        withSC3n j . send $ d_recv . synthdef (show n) . out (control KR "chan" 0)  $ envGen KR 1 1 0 1 RemoveSynth (envPerc' 0.001 (control KR "rel" 0.1) 1.0 (EnvNum 1, EnvNum 1)) *  (control KR "amp" 0) * orig
        where orig =  playBuf 2 AR (fromIntegral n) (0.5 + 1.5 * control KR "pitch" 0) 1 0 NoLoop RemoveSynth  


playSample c s t p v r  = withSC3n 57110 . sendBundle . bundle t . return $ 
                         s_new (show s) (-1) AddToTail 1 $ [("chan",fromIntegral (c*2)),("amp",v),("pitch",p),("rel",r)]
                               


initSamples :: String -> IO [String]
initSamples globs = do
        forM_ servers $ \i -> withSC3n i . send $ p_new [(1, AddToTail, 0)]
        ls <- namesMatching globs
	t <- time
        sequence_ $ do 
                j <- servers
                l@(i,_) <- zip [0..] ls
                return $  do 	
			bootSample j l
			playSample 0 i (t + 3 + fromIntegral i * 0.005) 0.5 0.1 (0.005)
	
	return ls

			
			
			
				
		

