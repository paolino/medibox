{-# LANGUAGE TypeFamilies, FlexibleInstances, NoMonomorphismRestriction #-}

module SC where

import Control.Arrow ((&&&))
import qualified Data.IntMap as IM
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad (forM_,when)

import Sound.OSC
import Sound.SC3 hiding (pitch)
import System.FilePath
import System.FilePath.Find
import System.Directory
import Control.Monad
import Data.Maybe
import System.Random


withSC3n :: Int -> Connection UDP a -> IO a
withSC3n i = withTransport (openUDP "127.0.0.1" $ fromIntegral i)

servers = [57110]
freqs = [440 * (1.059463)^^x | x <- [-40..400]]

quantf x ((y',y''):rs) 
	  | x <= y' && x - y' > y'' - x  = y''
	  | x <= y'  = y'
	  | otherwise = quantf x  rs

quantizefreq x = quantf x $ zip `ap` tail $ freqs

bootSin :: Int -> IO (Double -> Double -> Double -> Double ->  IO ())
bootSin j =  do 
	withSC3n j . send $ d_recv . synthdef "sin" . out 0 $ mce  [
		(lag (control KR "amp" 0) (control KR "rel" 0)) * sinOsc AR (lag ( control KR "pitch" 0) (control KR "rel" 0)) 0 
			* sinOsc AR (lag (control KR "pitch" 0) (control KR "rel" 0)) 0,
		(lag (control KR "amp" 0) (control KR "rel" 0)) * sinOsc AR (lag (control KR "pitch" 0 + 3) (control KR "rel" 0)) 0.3 * sinOsc AR (lag (control KR "pitch" 0) (control KR "rel" 0)) 0
		]
		
	withSC3n j . send $  s_new "sin" 1000 AddToTail 1 $ []
	threadDelay 1000
	return $  \t p a r -> withSC3n 57110 . sendBundle . bundle t . return $ 
                         n_set 1000  [("amp",a),("pitch",quantizefreq $ p),("rel",r)]



bootSinH :: Int -> IO (Double -> Double -> Double -> Double ->  IO ())
bootSinH j =  do 
	n <- randomIO :: IO Int
	withSC3n j . send $ d_recv . synthdef ("sinh" ++ show n) . out 0 $ 
		envGen KR 1 1 0 1 RemoveSynth (envPerc  0.001 (control KR "rel" 0.4)) *  (control KR "amp" 0.2) *mce  [
		sinOsc AR (control KR "pitch" 250) 0,
		sinOsc AR (control KR "pitch" 250 * 1.01) 0
		]
		
	return $  \t p a r -> do
			withSC3n 57110 . sendBundle . bundle t . return $ 
                         s_new ("sinh" ++ show n) (-1) AddToTail 1 [("amp",a),("pitch",quantizefreq $ p),("rel",r)]
bootSinHR :: Int -> IO (Double -> Double -> Double -> Double ->  IO ())
bootSinHR j =  do 
	n <- randomIO :: IO Int
	withSC3n j . send $ d_recv . synthdef ("sinh" ++ show n) . out 0 $ 
		envGen KR 1 1 0 1 RemoveSynth (envPerc  (control KR "rel" 0.4) 0.001) *  (control KR "amp" 0.2) *mce  [
		sinOsc AR (control KR "pitch" 250) 0,
		sinOsc AR (control KR "pitch" 250 * 1.05) 0
		]
		
	return $  \t p a r -> do
			withSC3n 57110 . sendBundle . bundle t . return $ 
                         s_new ("sinh" ++ show n) (-1) AddToTail 1 [("amp",a),("pitch",quantizefreq $ p),("rel",r)]


bootSample :: Int -> (Int,(FilePath,String)) -> IO ()
bootSample j (n,(fp,i)) = do
        withSC3n j . send $ b_free n
        withSC3n j . send $ b_allocRead n fp 0 0
        withSC3n j . send $ d_recv . synthdef i . out 0 $ envGen KR 1 1 0 1 RemoveSynth (envPerc 0.001 (control KR "rel" 0.1)) *  (control KR "amp" 0) * orig
        where orig =  playBuf 2 AR (fromIntegral n) (0.5 + 1.5 * control KR "pitch" 0) 1 0 NoLoop RemoveSynth  


playSample  t p v r s = withSC3n 57110 . sendBundle . bundle t . return $ 
                         s_new s (-1) AddToTail 1 $ [("amp",v),("pitch",p),("rel",r)]
                               


initSynths :: FilePath -> IO (Int -> Double -> Double -> Double -> Double ->  IO (), [Int])
initSynths sampledir = do
        putStrLn "Reading samples"
        forM_ servers $ \i -> withSC3n i . send $ p_new [(1, AddToTail, 0)]
        ls <- map (id &&& takeBaseName)  `fmap` (find (depth ==? 0) (extension ==? ".wav") sampledir)
        sequence_ $ do 
                j <- servers
                l <- zip [0..] ls
                return $ bootSample j l
	mapM_ print ls
	bass <- bootSin 57110
	pluck <- bootSinH 57110
	pluck2 <- bootSinHR 57110
	pluck3 <- bootSinH 57110
	let sel i t a p r 
		| i == 7 = bass t (25 + p * 50) a r
		| i == 6 = pluck t (25*8 + p * 50*8) a r
		| i == 5 = pluck2 t (25*16 + p * 50*16) a r
		| i < 7 = playSample t p a r . snd . fromJust . lookup i . zip [0..] $ ls
	return $ (sel , map fst . zip [0..] $ ls)

newtype Sequencer = Sequencer ([(Int,Double,Double,Double,Double)] -> IO Sequencer)

noteOut :: FilePath -> IO (Sequencer,[Int])
noteOut s = do
	(f,is) <- initSynths s
	let g ts js  = do
		t <- time
		let 	(t':rs) = dropWhile (<=t) ts
		forM_ js $ \(i,p,a,d,c) -> f i (t' + c  * 0.12 * 4) a p (d * 0.12 * 4) 
		t <- time
		when (t' > t) $ pauseThreadUntil t'
		return (Sequencer $ g rs)
	t0 <- time
	return $ (Sequencer $ g $ iterate (+ 0.12) t0,is)

prova = do 
	(s ,q) <- noteOut "/home/paolino/WAV/ByKit/Criteria"
	let 	cyc (Sequencer f) 0 = return ()
	 	cyc (Sequencer f) n =  do 
			s <- f [(fromIntegral n `mod` 4,1,0.8/128*fromIntegral n,0.4,0)]
			cyc s (n -1)
	cyc s 128
		 	
