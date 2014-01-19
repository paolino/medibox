{-# LANGUAGE ViewPatterns, TemplateHaskell, ScopedTypeVariables #-}
module Wave where

import Data.Binary

import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent

import Control.Arrow
import Control.Lens ((^.), at, traverse, (%~), _3,_4,(.~),_1,_2)
import Control.Lens.TH
import MidiComm
import qualified Data.Map as M

data Shape = Sine | SawP | SawN |Square deriving Enum

instance Binary Shape where
        put x = put (fromEnum x :: Int)
        get = do
                (i :: Int) <- get 
                return $ toEnum i

wave :: Shape -> Double -> Double
wave s t = let
	d = 2 * pi 
	n = t / d
	a = t - d * fromIntegral (floor n)
	in case s of
                SawP -> (a * 2 / d) - 1 
                SawN -> 1 - (a * 2 / d) 
                Sine -> sin a
                Square -> case a > pi of
                        True -> 1
                        False -> -1
data Wave = Wave {
        _shape :: Shape,
	_offset :: Int,
	_power :: Int,
	_comps :: M.Map Int (Double,Double,Double)
	}

$(makeLenses ''Wave)

instance Binary Wave where
	put (Wave sh o p co) = put sh >> put o >> put p >> put co
	get = do 
                sh <- get
		o <- get
		p <- get
		co <- get
		return (Wave sh o p co)


select 0 = _1
select 1 = _2
select 2 = _3

initWave :: M.Map Int (M.Map Int Wave)
initWave = M.fromList . zip [0..7] . repeat . M.fromList . zip [0..3] . repeat . Wave Square 0 64 . M.fromList . zip [0..1] $ repeat (0::Double,0,0)

clip x | x > 1 = 1
       | x < 0 = 0
       | otherwise = x

zpow x y 
        | x >= 0 = x ** y
        | otherwise = negate $  (negate x) ** y

evalWave (Wave sh o p xs) x  = clip $ (fromIntegral o/128 +) . sum $ map (flip zpow (0.5 + fromIntegral p/64)) [a/128 * wave sh (s/128*2*pi + x*w) | (w,s,a) <- M.elems xs]








mantain :: String -> TVar (M.Map Int (M.Map Int Wave)) -> IO ()
mantain s tw = do
  tp <- newTVarIO 0
  tc <- newTChanIO
  tfb <- newTChanIO 
  forkIO $ midiOut (s ++ ": feedback") 0 tfb
  forkIO $ midiIn (s ++ ": input") 0 tc
  
  forever . atomically $ do 
		(n,s) <- readTChan tc
                k <- readTVar tp

		if n<24 then do
                        let     (y,x) = n `divMod` 8
                                (l',w) = x `divMod` 2
			modifyTVar tw . flip M.adjust k . flip M.adjust l' . (comps %~) . flip M.adjust w $  select y .~ fromIntegral s
		else case n of
			25 ->	modifyTVar tw . flip M.adjust k . flip M.adjust 0 $ power .~ s
			24 ->	modifyTVar tw . flip M.adjust k . flip M.adjust 0 $ offset  .~ (s)
			27 ->	modifyTVar tw . flip M.adjust k . flip M.adjust 1 $ power .~ s
			26 ->	modifyTVar tw . flip M.adjust k . flip M.adjust 1 $ offset  .~ (s)
			29 ->	modifyTVar tw . flip M.adjust k . flip M.adjust 2 $ power .~ s
			28 ->	modifyTVar tw . flip M.adjust k . flip M.adjust 2 $ offset  .~ (s)
			31 ->	modifyTVar tw . flip M.adjust k . flip M.adjust 3 $ power .~ s
			30 ->	modifyTVar tw . flip M.adjust k . flip M.adjust 3 $ offset  .~ (s)
			32 ->	modifyTVar tw . flip M.adjust k . flip M.adjust 0 $ shape .~ toEnum s
			34 ->	modifyTVar tw . flip M.adjust k . flip M.adjust 1 $ shape  .~ toEnum s
			36 ->	modifyTVar tw . flip M.adjust k . flip M.adjust 2 $ shape  .~ toEnum s
			38 ->	modifyTVar tw . flip M.adjust k . flip M.adjust 3 $ shape  .~ toEnum s
                        -- change channel
			127 -> when (s < 8) $ do 
				writeTVar tp s
				w <- flip (M.!) s `fmap` readTVar tw
				forM_ [0 .. 23]  $ \n -> do 
                                        let     (y,x) = n `divMod` 8
                                                (l,wa) = x `divMod` 2 
                                        writeTChan  tfb $ (n,floor $ (flip (^.) comps (w M.! l) M.! wa) ^. select y)
				forM_ [0..3] $ \l -> do
                                        writeTChan tfb (25 + l * 2, flip (^.) power (w M.! l))
                                        writeTChan tfb (24 + l * 2, flip (^.) offset (w M.! l))
                                        writeTChan tfb (32 + l * 2, fromEnum $ flip (^.) shape (w M.! l))
			_ -> return ()
		
	
