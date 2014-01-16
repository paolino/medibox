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
import SC



data Wave = Wave {
	_offset :: Double,
	_amp :: Double,
	_quant :: Int,
	_power :: Int,
	_cut :: Double,
	_comps :: M.Map Int (Double,Double,Double)
	}

$(makeLenses ''Wave)

instance Binary Wave where
	put (Wave o a q p cu co) = put o >> put a >> put q >> put p >> put cu >> put co
	get = do 
		o <- get
		a <- get
		q <- get
		p <- get
		cu <- get
		co <- get
		return (Wave o a q p cu co)
freqs = [440 * (1.059463)^^x | x <- [-40..40]]

quant' x ((y',y''):rs) 
	  | x <= y' && x - y' > y'' - x  = y''
	  | x <= y'  = y'
	  | otherwise = quant' x  rs

quantize k x = quant' x $ zip `ap` tail $ [0,k..]


select 0 = _1
select 1 = _2
select 2 = _3

echo (k,0) = (k,"PITCH")
echo (k,1) = (k,"AMP")
echo (k,2) = (k,"RELEASE")
echo (k,3) = (k,"DELAY")
echo (k,4) = (k,"HARM")

main :: IO ()
main = do
  techo <- newTChanIO
  forkIO . forever $ atomically (readTChan techo) >>= print . echo
  w0 <- decodeFile "current.bb"
  let w1 = (M.fromList . zip [0..7] . repeat . M.fromList . zip [0..4] . repeat . Wave 0 1 0 2 0 . M.fromList . zip [0..7] $ repeat (0::Double,0,0))
  tw <- newTVarIO w0
  tp <- newTVarIO 0
  tl <- newTVarIO 0
  tc <- newTChanIO
  tfb <- newTChanIO 
  forkIO $ midiOut "sins_out_fb" 0 tfb
  forkIO $ midiIn "sins_in" 0 tc
  forkIO . forever . atomically $ do 
		(n',s) <- readTChan tc
		k <- readTVar tp
		l <- readTVar tl
		let n = n' + l * 24
		when (n<120) $ do
			modifyTVar tw . flip M.adjust k . flip M.adjust (n `div` 24) . (comps %~) . flip M.adjust (n `mod` 8) $  select (n `mod` 24 `div` 8) .~ fromIntegral s
		case n' of
			125 ->	do	k <- readTVar tp
					l <- readTVar tl
					modifyTVar tw . flip M.adjust k . flip M.adjust l $ cut .~ (fromIntegral s/128)

			124 ->	do	k <- readTVar tp
					l <- readTVar tl
					modifyTVar tw . flip M.adjust k . flip M.adjust l $ quant .~ s
			123 ->	do	k <- readTVar tp
					l <- readTVar tl
					modifyTVar tw . flip M.adjust k . flip M.adjust l $ amp .~ (fromIntegral s/128)
			122 ->	do	k <- readTVar tp
					l <- readTVar tl
					modifyTVar tw . flip M.adjust k . flip M.adjust l $ offset .~ (fromIntegral s/128)
			121 ->	do	k <- readTVar tp
					l <- readTVar tl
					modifyTVar tw . flip M.adjust k . flip M.adjust l $ power .~ s

			127 -> when (s < 8) $ do 
				writeTVar tp s
				w <- flip (M.!) s `fmap` readTVar tw
				l <- readTVar tl
				let g = l * 24
				forM_ [g .. g + 23]  $ \n -> writeTChan  tfb $ (n - g,floor $ (flip (^.) comps (w M.! (n `div` 24)) M.! (n `mod` 8)) ^. select (n `mod` 24 `div` 8))
				writeTChan tfb (125,floor $ flip (^.) cut (w M.! l) * 127)
				writeTChan tfb (124, flip (^.) quant (w M.! l))
				writeTChan tfb (123,floor $ flip (^.) amp (w M.! l) * 127)
				writeTChan tfb (122,floor $ flip (^.) offset (w M.! l) * 127)
				writeTChan tfb (121, flip (^.) power (w M.! l))
				-- writeTChan tfb (120, flip (^.) width (w M.! l))
			126 -> when (s < 5) $ do
				writeTVar tl s
				p <- readTVar tp
				w <- flip (M.!) p `fmap` readTVar tw
				let g = s * 24
				forM_ [g .. g + 23]  $ \n -> writeTChan  tfb $ (n - g,floor $ (flip (^.) comps (w M.! (n `div` 24)) M.! (n `mod` 8)) ^. select (n `mod` 24 `div` 8))
				writeTChan tfb (125,floor $ flip (^.) cut (w M.! s) * 127)
				writeTChan tfb (124, flip (^.) quant (w M.! s))
				writeTChan tfb (123,floor $ flip (^.) amp (w M.! s) * 127)
				writeTChan tfb (122,floor $ flip (^.) offset (w M.! s) * 127)
				writeTChan tfb (121, flip (^.) power (w M.! s))
				-- writeTChan tfb (120, flip (^.) width (w M.! s))
			_ -> return ()
		
		k <- readTVar tp
		l <- readTVar tl
		writeTChan techo (k,l)
  let ao l p 
	| p >= l = p
	| otherwise = 0
  (s ,q) <- noteOut "Prova"
  let cyc (Sequencer f) i =  do 
		es <- forM ([0..4] ++ [5,6,7]) $ \k -> do
			let x = fromIntegral i  * pi * 2 / 64 
			(fb,p,s,d,c) <- atomically $  do 
				let x = fromIntegral i  * pi * 2 / (2 ^ 8)
				ws <- flip (M.!) k `fmap` readTVar tw
				let [p,s,d,c,fb] = flip map [0..4] $ \r -> 
					let Wave o a q p l xs = ws M.! r
					in clip  . quantize ((fromIntegral q + 1)/128) . (o +) . (a *) . ao l $ sum $ map (^p) [a/4*tri (s/128*2*pi + x*w)/10 | (w,s,a) <- M.elems xs]
				return (fb,p,s,d,c)
			return $ (k,fb,p,s,d,c)
		print es
		s <- f es
		cyc s (i + 1)
		  
  forkIO $ cyc s 0
  getLine
  ws <- atomically $ readTVar tw
  encodeFile "current.bb" ws

clip x | x > 1 = 1
       | x < 0 = 0
       | otherwise = x
  		
tri t = let
	d = 2 * pi 
	n = t / d
	a = t - d * fromIntegral (floor n)
	in (a * 2 / d) - 1
	
