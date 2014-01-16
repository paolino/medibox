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



collapse 0 = 0
collapse 1 = 0
collapse 2 = 2
collapse 3 = 3
collapse 4 = 3
collapse 5 = 5
collapse 6 = 7
collapse 7 = 7
collapse 8 = 8
collapse 9 = 7
collapse 10 = 12
collapse 11 = 12
modcollapse x = let
        (o,n) = x `divMod` 12
        in o * 12 + collapse n

withSC3n :: Int -> Connection UDP a -> IO a
withSC3n i = withTransport (openUDP "127.0.0.1" $ fromIntegral i)

servers = [57110]
freqs y = [440 * (1.059463)^^x | x <- map (subtract 48 . (+y) . modcollapse) [0 .. 300]]

quantf _ [] = 0
quantf x ((y',y''):rs) 
	  | x <= y' && x - y' > y'' - x  = y''
	  | x <= y'  = y'
	  | otherwise = quantf x  rs

quantizefreq x = quantf x $ zip `ap` tail $ freqs 40
quantizefreq' f x = quantf x $ zip `ap` tail $ freqs f

bootSin :: Int -> IO (Double -> Double -> Double -> Double ->  IO ())
bootSin j =  do 
	withSC3n j . send $ d_recv . synthdef "sin" . out 0 $ 
		(envGen KR 1 1 0 1 RemoveSynth (envPerc  0.001 (control KR "rel" 0.4)) * (control KR "amp" 0) * sinOsc AR ( control KR "pitch" 0 * 4) 0)
		+ 
		mce  [
		-- envGen KR 1 1 0 1 RemoveSynth (envPerc  0.001 (control KR "rel" 0.4)) * (control KR "amp" 0) * sinOsc AR ( control KR "pitch" 0 * 4) 0,
		(lag (control KR "amp" 0) (control KR "rel" 0)) * sinOsc AR (lag ( control KR "pitch" 0) (control KR "rel" 0)) 0 
			* sinOsc AR (lag (control KR "pitch" 0) (control KR "rel" 0)) 0,
		(lag (control KR "amp" 0) (control KR "rel" 0)) * sinOsc AR (lag (control KR "pitch" 0 + 3) (control KR "rel" 0)) 0.3 * sinOsc AR (lag (control KR "pitch" 0) (control KR "rel" 0)) 0
		]
	withSC3n j . send $ n_free [2000]	
	withSC3n j . send $  s_new "sin" 2000 AddToTail 1 $ []
	threadDelay 100000
	return $  \t p a r -> withSC3n 57110 . sendBundle . bundle t . return $ 
                         n_set 2000  [("amp",a),("pitch",quantizefreq $ p),("rel",r)]



bootSinH :: Int -> IO (Double -> Double -> Double -> Double -> Double ->  IO ())
bootSinH j =  do 
	n <- randomIO :: IO Int
	withSC3n j . send $ d_recv . synthdef ("sinh" ++ show n) . out 0 $ 
		envGen KR 1 1 0 1 RemoveSynth (envPerc  0.001 (control KR "rel" 0.4)) *  (control KR "amp" 0.2) *mce  [
		sinOsc AR (control KR "pitch" 250) 0,
		sinOsc AR (control KR "pitch" 250 * 1.01) 0
		]
		
	return $  \t fb p a r -> do
			withSC3n 57110 . sendBundle . bundle t . return $ 
                         s_new ("sinh" ++ show n) (-1) AddToTail 1 [("amp",a),("pitch",quantizefreq' (floor fb) $ p),("rel",r)]
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
                               


initSynths :: FilePath -> IO (Int -> Double -> Double -> Double -> Double -> Double ->  IO (), [Int])
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
	let sel i t fb a p r 
		| i == 7 = pluck t (fb * 128) (50 + p * 50*2) a r
		| i == 6 = pluck t (fb * 128)  (25*8 + p * 50*8) a r
		| i == 5 = pluck t (fb * 128) (25*16 + p * 50*16) a r
		| i < 5 = do 
			t <- time
			playSample t p a r . snd . fromJust . lookup i . zip [0..] $ ls
	return $ (sel , map fst . zip [0..] $ ls)

newtype Sequencer = Sequencer ([(Int,Double,Double,Double,Double,Double)] -> IO Sequencer)

noteOut :: FilePath -> IO (Sequencer,[Int])
noteOut s = do
	(f,is) <- initSynths s
	let g ts js  = do
		t <- time
		let 	(t':rs) = dropWhile (<=t) ts
		forM_ js $ \(i,fb,p,a,d,c) -> f i (t' + c  * 0.12*128) fb a p (d * 0.12 * 4) 
		t <- time
		when (t' > t) $ pauseThreadUntil t'
		return (Sequencer $ g rs)
	t0 <- time
	return $ (Sequencer $ g $ iterate (+ 0.12) t0,is)


