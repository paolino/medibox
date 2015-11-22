
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
import Control.Arrow
import Data.Traversable

import TestAlsa
import Board
type Controls = M.Map Int Int
readValue :: Int -> Double -> Double -> Controls -> Double
readValue i f s = (+s) . (*f) . (/128) . fromIntegral . M.findWithDefault 0 i

data Morph = Morph {
  _shiftNote :: Int 
  , _zoomVolume ::Double
  }deriving (Show,Read)

makeLenses ''Morph


type TrackId = Int

readMorph :: TrackId -> Controls -> Morph
readMorph n m = Morph 
  (floor $ readValue n 50 0 m) 
  (readValue (24 + n) 1 0 m)
        

morph :: Morph -> E N -> E N
morph (Morph dn vo) = over event $ \(N n v d) -> 
    N (n + dn) (floor (vo * fromIntegral v)) d

data Displace = Displace {
  _shiftTime :: Time , 
  _expandTime :: Time 
  }deriving (Show,Read)

makeLenses ''Displace

displace :: Displace -> E a -> E a
displace (Displace dt ro) = over timet ((+dt) . (*ro)) where

readDisplace n m =  Displace 
  (readValue (16 + n) 4 0 m) 
  (readValue (8 + n)  2 0.5 m)
  
data Track = Track {
  _morpher :: Morph,
  _displacer ::  Displace,
  _eventi :: [E N]
  } deriving (Show,Read)

makeLenses ''Track 



dispatch :: Track -> [E N]
dispatch (Track m d es ) = map (displace d . morph m) es 
    

updateTrack ::  Controls -> TrackId -> Track -> Track
updateTrack m i (Track  _ _ es ) = Track (readMorph i m ) (readDisplace i m)  es





-- listen to notes and substitute the nearest event
updateNotes :: M.Map Int Int -> [Track] -> Board NE 
updateNotes ctrls  = concatMap convert . concatMap dispatch . zipWith (updateTrack ctrls) [0..]  


data L = LR R | P | T deriving Read 
data R = R Int Int Int Int Int Int Time Time deriving Read 

randomNotes :: R -> IO [E N]
randomNotes (R s n p0 dp v0 dv dt dl) = do
    setStdGen (mkStdGen s)
    forM [1 .. n] $ \_ -> do
      p <- randomRIO (p0,p0 + dp)
      v <- randomRIO (v0,v0 + dv)
      t <- randomRIO (0,dt)
      dl <- randomRIO (0,dl)
      return $ E (N p v dl) t 


l = LR (R 1 5 48 5 30 80 2 0.5)

input  tracks exit = forever $ do 
      l <- getLine
      join . atomically $ case reads l of 
                      [(T,_)] -> return $ readTVarIO tracks >>= mapM_ print
                      [(LR r,_)] -> do
                        return $ do 
                                  es <- randomNotes $ r 
                                  atomically $  
                                          modifyTVar tracks $ map (set eventi es)
                                  
                      _ ->   writeTChan exit () >> return (return ())

pg = [E (N 50 50 0.8) 0 , E (N 60 50 0.8)  0.25 ,E (N 40 50 0.8) 0.65 , E (N 70 50 0.8) 0.5 , E (N 90 50 0.8) 0.75] 

main = do
  board <- newTVarIO [] 
  cmap <- newTVarIO M.empty 

  tracks <- newTVarIO $ replicate 8 $ Track  (Morph 0 100) (Displace 0 1) pg 
  u <- newTChanIO 
  forkIO . forever . atomically $ do
      () <- readTChan u
      c <- readTVar cmap
      ts <- readTVar tracks 
      writeTVar board (updateNotes c ts)
      
    
  exit <- newTChanIO
  forkIO $ sequp u board cmap
  forkIO $ input tracks exit
  atomically $ readTChan exit
