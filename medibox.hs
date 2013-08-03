{-# LANGUAGE NoMonomorphismRestriction #-}       
import Control.Concurrent.STM
import Control.Concurrent
import Data.List 

import qualified Data.IntMap as IM
import Control.Arrow
import Control.Monad

import System.IO 
import Control.Lens

import Sound.OSC (sleepThreadUntil, time)
------
import Sequenza
import Supercollider
import Midi


delay = 0.01
sampledir = "."
     
data Event = Event 
        { etrack :: Int
        , etime :: Tempo
        , esound :: (Int, Double, Int)
        }
        deriving (Show,Read)

main = do 
        play <- initSamples  sampledir
        tmp <- newTVarIO $ (1,120,0,0)
        let tracks = zip [0..ntracks - 1] $ repeat (noseq Base)
        thypes <- newTVarIO $ IM.fromList tracks 
        tupdate <- newTChanIO 
        tsupdate <- newTChanIO 
        tdeps <- newTVarIO [] 
        tprog <- newTVarIO $ (Tracks 0)
        tevents <- newTVarIO []
        tstartcycle <- newTVarIO 0
        forkOS $ midiIn thypes tprog tupdate tsupdate tmp tdeps
        forkOS $ midiOut thypes tprog tupdate
        let     fw = 256 -- fire window
                w = 4 -- pattern cycle
        let  updateTEvents = forever . atomically $ do 
                        -- wait for interface change a track parameter 
                        _ <- readTChan tsupdate
                        trs <- readTVar thypes
                        let   mp = fmap (_pres) trs
                        let   f (n, s@(Sequenza pres sa pi da shift qm)) = map (\(t,a) -> Event n t (sa,a,pi)) (sequenza mp s) 
                        writeTVar tevents $ concatMap f (IM.assocs trs)

             fireEvents _ [] = return ()
             fireEvents t0 ((t1,t2):ts) = do    
                        sleepThreadUntil (t0 + t1) -- wait next tick
                        putStr "." >> hFlush stdout
                        let     window f = filter (\x -> f x >= t1 && f x < t2)
                        ps <- atomically $ fmap (window ((*w) . etime)) $ readTVar tevents
                        forM_ ps $ \(Event _ t (sa,a,pi)) -> play sa (t0 + w*t + delay) a (1 + from128p pi)
                        fireEvents t0 ts
             updateCycle t n = do
                        sleepThreadUntil (t + n * w)
                        forkIO $ fireEvents (t + n * w) $ take fw $ ap zip tail [0,w/fw..]
                        updateCycle t (n + 1)
        
        now <- time 
        forkIO $ updateCycle now 0
        updateTEvents
        


