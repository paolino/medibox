
{-# LANGUAGE ViewPatterns, ScopedTypeVariables, TemplateHaskell #-}
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Control.Exception
import Sound.OSC
import qualified Data.Map as M
import Control.Lens
import Control.Lens.TH
import Data.List 
import Data.Ord
import System.Random
import Control.Monad.Random
import Control.Arrow
import Data.Traversable
import System.Console.Haskeline

import TestAlsa
import Board

type Controls = M.Map Int Int

readValue :: Int -> Double -> Double -> Controls -> Double
readValue i f s = (+s) . (*f) . (/128) . fromIntegral . M.findWithDefault 0 i

type TrackId = Int
        


data Displace = Displace {
  _shiftTime :: Time , 
  _expandTime :: Time 
  }deriving (Show,Read)

displace :: Displace -> E a -> E a
displace (Displace dt ro) = over timet ((+dt) . (*ro)) where

readDisplace n m =  Displace 
  (readValue (0 + n) 4 0 m) 
  (readValue (8 + n)  2 0 m)
  
data Repeat = Repeat {
  _repeatSubd :: Int,
  _repeatCount :: Int
  } deriving (Show,Read)

repea ::  Repeat  -> Int -> Int -> E a -> E a
repea (Repeat n s) i k  = over timet (+ (fromIntegral k / (fromIntegral s + 1) / (fromIntegral n + 1))) . over timet (+ (fromIntegral i / (fromIntegral n + 1)))


readRepeat n m = Repeat
  (floor (readValue (16 + n) 128 0 m))
  (floor (readValue (24 + n) 128 0 m))


data R = R Int Int Int Int Int Int Time deriving (Read, Show)

randomness :: Int -> R -> [E N]
randomness c (R seed n  p dp v dv dt) = flip evalRand (mkStdGen seed) . replicateM n $ do
          p <- getRandomR (p,p + dp)
          v <- getRandomR (v,v + dv)
          t <-  getRandomR(0,1)
          dt <- getRandomR (0,dt)
          return $  E (N c p v dt) t

readRandomness n  m = R
    (floor (readValue (32 + n) 128 0 m))
    (floor (readValue (40 + n) 128 0 m))
    (floor (readValue (48 + n) 128 0 m))
    (floor (readValue (56 + n) 128 0 m))
    (floor (readValue (64 + n) 128 0 m))
    (floor (readValue (72 + n) 128 0 m))
    (readValue (80 + n) 1 0 m)

data Track = Track {
  _displacer ::  Displace,
  _repeater :: Repeat , 
  _randomnesser :: R
  } deriving (Show,Read)

makeLenses ''Track 


events :: Int -> Track -> [E N]
events c (Track d r@(Repeat n s) ra) = [0..n] >>= \n -> [0..s] >>= f n where
  es = randomness c ra
  l = length es
  es' = take ((l `div` (n + 1) `div` (s + 1)) + 1) es
  f i k = map (repea r i k . displace d ) es'
    

fromControls ::  Controls -> Int -> Track
fromControls  m i = Track (readDisplace i m)  (readRepeat i m) (readRandomness i m)

-- listen to notes and substitute the nearest event
board :: M.Map Int Int -> Board N
board m  =  [0 ..7] >>= (events `ap` (fromControls m)) 


data L =  P | T | Q deriving Read 

pg c = [E (N c 50 50 0.8) 0 , E (N c 60 50 0.8)  0.25 ,E (N c 40 50 0.8) 0.65 , E (N c 70 50 0.8) 0.5 , E (N c 90 50 0.8) 0.75] 

main = do

  forkIO $ sequp board

  exit <- newTChanIO
  forkIO . runInputT defaultSettings $ forever $ do 
      l <- getInputLine "> "
      liftIO . atomically $ case reads <$> l of 
                      Just [(Q,_)] ->   writeTChan exit () 
                      _ -> return ()

  atomically $ readTChan exit
