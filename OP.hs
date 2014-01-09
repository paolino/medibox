{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import Data.IORef

import Graphics.UI.Gtk as Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Cairo
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk (AttrOp((:=)))
import Graphics.UI.Gtk.OpenGL as GtkGL
import Control.Monad
import System.Random
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad.Trans
import Sound.OSC
import Graphics.Rendering.OpenGL as GL

import Control.Lens ((^.), at, traverse, (%~), _3,_4,(.~),_1,_2)
import MidiComm
import qualified Data.Map as M


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

select 0 = _1
select 1 = _2
select 2 = _3

main :: IO ()
main = do
  tw <- newTVarIO (M.fromList . zip [0..3] . repeat . M.fromList . zip [0..7] $ repeat (0::GLfloat,0,0))
  te <- newTVarIO (\_ -> 0)
  tc <- newTChanIO
  tn <- newTChanIO
  forkIO $ midiIn "sins_in" 0 tc
  forkIO $ midiOutNote "sins_out" tn
  forkIO . forever $ do 
		(n,s) <- atomically (readTChan tc) 
		when (n<96) $ 
			atomically . modifyTVar tw . flip M.adjust (n `div` 24) . flip M.adjust (n `mod` 8) $  select (n `mod` 24 `div` 8) .~ fromIntegral s
  
  let lo i = do
  	dp <- newTVarIO (0,0,0,0)
	forkIO $ do 
		threadDelay $ 120 * 1000
		forkIO $ lo $ i + 1
		(c,p,s,d) <- atomically $ readTVar dp
		threadDelay $ floor $ fromIntegral (floor c) / 8  * 120 * 1000
		atomically $ writeTChan tn (0,modcollapse $ floor p,floor s,True)
		threadDelay $ floor $ fromIntegral (floor d) / 8 * 120 * 1000
		atomically $ writeTChan tn (0,modcollapse $ floor p,0,False)
		
	let x = fromIntegral i  * pi * 2 / 64 
	atomically $ do 
		ws <- readTVar tw
		let c = (128 *)  $ sum $ map (^2) [a/16*sin (s/128*2*pi + x*w)/10 | (w,s,a) <- M.elems (ws M.! 3)]
		let p = (128 *)  $ sum $ map (^2) [a/16*sin (s/128*2*pi + x*w)/10 | (w,s,a) <- M.elems (ws M.! 0)]
		let s = (128 *)  $ sum  $ map (^2) [a/16*sin (s/128*2*pi + x*w)/10 | (w,s,a) <- M.elems (ws M.! 1)]
		let d =(128 *) $ sum $ map (^2) [a/16*sin (s/128*2*pi + x*w)/10 | (w,s,a) <- M.elems (ws M.! 2)]
		c `seq` d `seq` p `seq` s `seq` writeTVar dp (c,p,s,d)

  
  lo 0 
  getLine
  return ()
		
