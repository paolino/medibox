{-# LANGUAGE TypeFamilies, FlexibleInstances, NoMonomorphismRestriction #-}

module SC where

import Control.Arrow ((&&&))
import qualified Data.IntMap as IM
import Control.Concurrent.STM
import Control.Monad (forM_,when)

import Sound.OSC
import Sound.SC3 hiding (pitch)
import System.FilePath
import System.FilePath.Find
import System.Directory

import Data.Maybe


withSC3n :: Int -> Connection UDP a -> IO a
withSC3n i = withTransport (openUDP "127.0.0.1" $ fromIntegral i)

servers = [57110]


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
	return $ (\i t a p r -> playSample t p a r . snd . fromJust . lookup i . zip [0..] $ ls, map fst . zip [0..] $ ls)

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
		 	
