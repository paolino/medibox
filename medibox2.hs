{-# LANGUAGE NoMonomorphismRestriction #-}       
import Control.Concurrent.STM
import Control.Concurrent
import Data.List 

import qualified Data.IntMap as IM
import Control.Arrow
import Control.Monad

import System.IO 
import Control.Lens

import Sound.OSC (sleepThreadUntil, time, sleepThread)
------
import Sequenza
import Supercollider
import Interface
import Haskell
import Realize
import Dynamic
import GUI
import MidiComm
import Projections


delay = 0.01
sampledir = "."
    
regchan = 0 
clientname = "medibox midi"

weight = 127 
main = do 
        ssamples2Playable <- initSamples sampledir
        tpresence <- newTVarIO $ Interface (IM.fromList $ zip [0..127] $ repeat (Base zero, Nothing)) (IM.fromList $ zip [0..127] $ repeat zero)
                        (IM.fromList $ zip [0..127] $ repeat zero)
        -- communication patch bay
        (midiin, midiout, killmidi) <- midiInOut  clientname regchan 
        midiin2 <- atomically $ dupTChan midiin
        guiIn <- newTChanIO
        guiOut <- newTChanIO
        interfaceIn <- newTChanIO
        forkIO . forever . atomically $ do
                s <- readTChan midiin `orElse` readTChan guiOut
                writeTChan interfaceIn s
                writeTChan midiout s
        interfaceOut <- newTChanIO
        forkIO . forever . atomically $ do
                s <- readTChan interfaceOut 
                writeTChan midiout s
                writeTChan guiIn s
        forkIO . forever . atomically $ do
                s <- readTChan midiin2 
                writeTChan guiIn s
        --------------------------------------------------------------

        (killInterface, tselection) <- interface interfaceIn interfaceOut tpresence
        -- outputCycle :: Tempo -> (Int -> IO (Score Double)) -> IO Globals -> IO [Playable] -> IO ()
        t0 <- time
        let serveProj i = atomically $ do
                        Interface seqs projs ssamples <-  readTVar tpresence
                        let     proj = projs IM.! i
                                (rs,nseqs) = project seqs proj
                        writeTVar tpresence $ Interface nseqs projs ssamples
                        return rs
            players = atomically $ do 
                        Interface seqs projs ssamples <-  readTVar tpresence
                        return $ map ssamples2Playable $ IM.elems ssamples
                
        v <- readFile  "current.medibox"
        atomically $ writeTVar tpresence $ read v
        forkIO $ outputCycle  (t0 + 1) serveProj (return $ Globals 0.1 4 100) players 
        forkIO . forever $ do 
                sleepThread 5
                v  <- atomically $ readTVar tpresence
                writeFile "current.medibox" $ show v
                
        gui guiIn guiOut tpresence tselection

         

        
        --forkIO $ directControl readCInterface
        --outputCycle now readGlobals readInterface 
        


