

{-# LANGUAGE TypeFamilies, FlexibleInstances, NoMonomorphismRestriction #-}

module Samples where

import Control.Monad (forM_,when)

import Sound.OSC
import Sound.SC3 hiding (pitch)
import System.FilePath
import System.FilePath.Glob
import System.Directory
import Data.List
import Control.Monad
import Debug.Trace
import qualified Data.Map as M
quantf _ [] = 0
quantf x ((y',y''):rs) 
	  | x <= y' && x - y' > y'' - x  = y''
	  | x <= y'  = y'
	  | otherwise = quantf x  rs



quantize2 k y x = let 
        xn = [takeWhile (<30) . dropWhile (< 0.01) $ sort [3 ^^ x / 2 ^^ y| x <- [-kn .. kn], y <- [-12 .. 12]] | kn <- [0 .. 7]]
        in quantf x $ zip `ap` tail $ map (*y) $ xn !! k


projectLin  x0 x1 k = x0 + (x1-x0)*k

-- projectLog :: UGen -> UGen -> UGen -> UGen
projectLog x0 x1 k = (x1 - x0)/log 2*log (1 + k) + x0
projectExp x0 y0 k = (y0 - x0)/(exp 5 - 1)*(exp (5*k) - 1) + x0

withSC3n :: Int -> Connection UDP a -> IO a
withSC3n i = withTransport (openUDP "127.0.0.1" $ fromIntegral i)

servers = [57110 .. 57117]

bootSample :: Int -> (Int,FilePath) -> IO ()
bootSample j (n,fp) = do
        withSC3n j . send $ b_free n
        withSC3n j . send $ b_allocRead n fp 0 0
        withSC3n j . send $ b_free (n + 1)
        withSC3n j . send $ b_allocReadChannel (n + 1) fp 0 0 [0]
        withSC3n j . send $ d_recv . synthdef (show $ n `div` 2) . out 0  $  
		(envGen KR 1 1 0 1 RemoveSynth (envPerc' (fckr 0 15 / 10) (control KR "rel" 0.1 - fckr 0 15/ 10) 1.0 (EnvNum 1, EnvNum 1)) *  (control KR "amp" 0) * (orig' + (sum $ map orig'' [0..2])))
	where 	
		orig' =  (control KR "23" 0) * playBuf 2 AR (fromIntegral n) (0.5 + 1.5 * control KR "pitch" 0) 1 0 NoLoop DoNothing 
		{-
		orig = 0.33 * sum [
			bPeakEQ (orig'')
			(projectExp 40 10000 $ control KR (show $ x ) 0) 
			(projectExp 0.1 100 $ control KR (show $ x + 8) 0) 
			(projectLin (-36) 36 $ control KR (show $ x + 16) 1)
			| x <- [0..2]
			]
		-}
		fckr i = flip (control KR) 0 . show . (+ 0)
		fckr' i = flip (control KR) 0 . show . (+ 8*i)
		orig'' i = {-  (envGen KR 1 1 0 1 DoNothing (envPerc' 0.001 (control KR "rel" 0.1 * (fckr i 4)) 1.0 (EnvNum (-1), EnvNum (6)))) -} 
				
				tGrains 2 
				(impulse AR (1000 * fckr' i 0)  0) --freq
				(fromIntegral $ n + 1)
				1
				(fckr i 3 * bufDur KR (fromIntegral $ n + 1))
					
				(projectExp 0.002 0.1 $ fckr i 1 * (1 + fckr i 2 * sinOsc AR (1000 * (fckr' i 0 + fckr i 4/1000)) 0))
				0 
				(fckr' i 5)
				4
playSample c s t p v r ps ap co =  withSC3n (57110 + c) . sendBundle . bundle t . return $ 
                         s_new (show s) (-1) AddToTail 1 $ [("amp",v),("pitch",p),("rel",r)] ++ 
				(map (\(i,v) -> (show i,v)) $ M.assocs . M.adjust (quantize2 ap co) 0 . M.adjust (quantize2 ap co) 8 . M.adjust (quantize2 ap co) 16 $ ps)
                               
{-
BPeakEQ.ar(
		BrownNoise.ar([1,1]		),
    MouseX.kr(20, 20000, \exponential),
    0.01, // rq
    MouseY.kr(12.0, -12.0, \linear),
    0.5); // mul
-}

initSamples :: String -> IO [String]
initSamples globs = do
        forM_ servers $ \i -> withSC3n i . send $ p_new [(1, AddToTail, 0)]
        ls <- sort `fmap` namesMatching globs
	t <- time
        sequence_ $ do 
                j <- servers
                l@(i,_) <- zip [0,2 ..] ls
                return $  do 	
			bootSample j l
			-- playSample 0 i (t + 3 + fromIntegral i * 0.005) 0.5 0.1 (0.005) []
	
	return ls

			
			
			
				
		

