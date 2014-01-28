
{-# LANGUAGE TypeFamilies, FlexibleInstances, NoMonomorphismRestriction #-}

-- module Supercollider where

import Control.Arrow ((&&&))
import qualified Data.IntMap as M
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad (forM_,when)

import Sound.OSC
import Sound.SC3 hiding (pitch)
import System.FilePath
import System.FilePath.Glob
import System.Directory
import Control.Monad
import Data.Maybe
import System.Random

import MidiComm
import Debug.Trace

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

withSC3n :: Int -> Connection UDP a -> IO a
withSC3n i = withTransport (openUDP "127.0.0.1" $ fromIntegral i)

servers = [57110]

bootSample :: Int -> (Int,FilePath) -> IO ()
bootSample j (n,fp) = do
        withSC3n j . send $ b_free n
        withSC3n j . send $ b_allocRead n fp 0 0
        withSC3n j . send $ d_recv . synthdef (show n) . out 0 $ envGen KR 1 1 0 1 RemoveSynth (envPerc' 0.001 (control KR "rel" 0.1) 1.0 (EnvNum 1, EnvNum 1)) *  (control KR "amp" 0) * orig
        where orig =  playBuf 2 AR (fromIntegral n) (0.5 + 1.5 * control KR "pitch" 0) 1 0 NoLoop RemoveSynth  


playSample  s t p v r  = withSC3n 57110 . sendBundle . bundle t . return $ 
                         s_new (show s) (-1) AddToTail 1 $ [("amp",v),("pitch",p),("rel",r)]
                               


initSynths :: String -> IO [String]
initSynths globs = do
        forM_ servers $ \i -> withSC3n i . send $ p_new [(1, AddToTail, 0)]
        ls <- namesMatching globs
	t <- time
        sequence_ $ do 
                j <- servers
                l@(i,_) <- zip [0..] ls
                return $  do 	
			bootSample j l
			playSample i (t + 3 + fromIntegral i * 0.005) 0.5 0.1 (0.005)
	
	return ls

newtype Sequencer = Sequencer ((Time -> Double -> IO ()) -> IO Sequencer)

quantize t0 dt d't = t0 + (fromIntegral (floor $ dt/d't)  + 1) * d't
sequencer :: Time -> Time -> TVar Integer -> TVar Integer -> IO (Sequencer,TVar Integer,TVar Double)
sequencer ot t0 tp td  = do
	tz <- newTVarIO 34
	tq <- newTVarIO 1
	let g v t f  = do
		let dt = t - t0
		(t',v') <- atomically $ do
			v0 <- readTVar tz
			q <- readTVar tq
			p <- readTVar tp
			d <- readTVar td
			let 	
				(n,z) = (d*v +  p) `divMod` 128
				
				-- v'= z
				z' = if z < 15 then 15 else z
				v' = (head $ dropWhile (<=z') $ iterate (*2) 1)
				d't = fromIntegral v'/1024
				timev' = fromIntegral n * d't + quantize t0 dt d't
				nexttick = quantize t0 dt q
				(tev,val) = if nexttick > timev' + 1/64 then (timev',z) else (nexttick,v0)
				
			return $ trace (show (tev,val)) $  (tev, val)
		f  t' (fromIntegral v'/ 128)
		pauseThreadUntil t'
		return (Sequencer (g v' t'))
	return (Sequencer (g 34 t0), tz, tq) 

samplesequencer :: Time -> Time -> Integer -> IO (TVar Integer,TVar Integer, TVar Integer, TVar Integer,TVar Double,TVar Integer,TVar Double)
samplesequencer ot t0 d = do
	tp <- newTVarIO 43
	td <- newTVarIO d
	tv <- newTVarIO 0
	tf <- newTVarIO 0
	tsa <- newTVarIO 0
	(s,tz,tq) <- sequencer ot t0 tp td  
	let loop n (Sequencer f)  = do
		f' <- atomically $ do 
			d <- readTVar td
			v <- readTVar tv
			fh <- readTVar tf
			sa <- readTVar tsa
			return $ \t r -> playSample sa (t + 0.2) 0.5 (r*v) (4*r)
		f f' >>= loop (n + 1)
	forkIO $ loop 0 s
	return (tf,tsa,tp,td,tv,tz,tq)
unzip4 = foldr f ([],[],[],[]) where
		f (x,y,z,g) (xs,ys,zs,gs) = (x:xs,y:ys,z:zs,g:gs)
unzip5 = foldr f ([],[],[],[],[]) where
		f (x,y,z,g,k) (xs,ys,zs,gs,ks) = (x:xs,y:ys,z:zs,g:gs,k:ks)
unzip6 = foldr f ([],[],[],[],[],[]) where
		f (x,y,z,g,k,w) (xs,ys,zs,gs,ks,ws) = (x:xs,y:ys,z:zs,g:gs,k:ks,w:ws)
unzip7 = foldr f ([],[],[],[],[],[],[]) where
		f (x,y,z,g,k,w,r) (xs,ys,zs,gs,ks,ws,rs) = (x:xs,y:ys,z:zs,g:gs,k:ks,w:ws,r:rs)
main = do 
	ls <- initSynths "/home/paolino/WAV/*/*/*.wav"
	t <- time
	(tf,tsa,tp,td,tv,tz,tq) <- unzip7 `fmap` (replicateM 8 $ samplesequencer 1 t 64)
	mc <- newTChanIO
	forkIO $ midiIn "samples" 0 mc
	forever $ do 
			atomically $ do
				(n,s) <- readTChan mc
				let (x,y) = n `divMod` 8
				when (x < 7) $ case x of 	
					0 -> modifyTVar (tsa !! y) $ const . fromIntegral $ s
					1 -> modifyTVar (tp !! y) . const $ fromIntegral s
					2 -> modifyTVar (td !! y) . const $ fromIntegral s 
					3 -> modifyTVar (tv !! y) . const $ fromIntegral s / 128
					4 -> modifyTVar (tq !! y) . const $ (fromIntegral s + 1) / 16
					5 -> modifyTVar (tz !! y) . const $ fromIntegral s 
					_ -> return ()
			
			
			
				
		

