{-# LANGUAGE ViewPatterns, TemplateHaskell #-}
module Main (main) where

import Data.Binary

import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent
import Sound.OSC

import Control.Arrow
import Control.Lens ((^.), at, traverse, (%~), _3,_4,(.~),_1,_2)
import Control.Lens.TH
import MidiComm
import qualified Data.Map as M
import Supercollider
import System.IO.Unsafe

import Wave

main :: IO ()
main = do
  w0 <- decodeFile "current.bb"
  -- let w0 = initWave
  tw <- newTVarIO w0
  har <- newTVarIO (0.2,0.5,M.fromList $ zip [0..] $ replicate 8 . M.fromList . zip [0..23] $ repeat 0)
  forkIO $ mantain "player" tw har
  (s ,q) <- noteOut $  ["Samples/hiha.wav","Samples/snare.wav","Samples/kick.wav","Samples/perc1.wav","Samples/perc2.wav"]
  let cyc (Sequencer f) i =  do 
		es <- forM [0..7] $ \k -> do
                        (ha,fb,wns) <- atomically $ readTVar har
			[p,s,d,c] <- atomically $  do 
				let x = fromIntegral i  * pi * 2 / 32
				ws <- flip (M.!) k `fmap` readTVar tw
				return $ map (\r -> evalWave (ws M.! r) x)  [0..3]
			return $ (k,p,s,d,c,ha,fb,wns M.! k)
		s <- f es
		cyc s (i + 1)
  forkIO $ cyc s 0
  getLine
  ws <- atomically $ readTVar tw
  encodeFile "current.bb" ws

clip x | x > 1 = 1
       | x < 0 = 0
       | otherwise = x
  		
	
