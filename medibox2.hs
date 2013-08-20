{-# LANGUAGE NoMonomorphismRestriction #-}       
import Control.Concurrent.STM
import Control.Concurrent
import Data.List 
import System.Directory

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
import Instr


delay = 0.01
sampledir = "."
    
regchan = 0 
clientname = "medibox midi"

weight = 127 
main = do 
        (msounds, scontrol2Playable,ssamples2Playable) <- initSynths sampledir
        tpresence <- newTVarIO $ Interface 
                        (IM.fromList $ zip [0..127] $ repeat (Base zero, Nothing)) 
                        (IM.fromList $ zip [0..127] $ repeat zero)
                        (IM.fromList $  do 
                                i <- [0 .. 15]
                                j <- [0 .. 6]
                                if j == 0 then [(i*7,Realize (i*7,i*7) 
                                        (Right $ Synth i 16 0 (i*7 + 1) (i*7 + 2) (i*7 + 3) (i*7 + 4) (i*7 + 5) (i*7 + 6)))]
                                        else [(i*7 + j, Realize (i*7 + j, i*7 + j) (Left $ SControl $ (i * 7 + j)))]) 
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
        let serveProj (pi,si) = atomically $ do
                        Interface seqs projs ssamples <-  readTVar tpresence
                        let     proj = projs IM.! pi
                                
                        case querySequenza seqs si of
                                Nothing -> return []
                                Just (_,sc,nseqs) -> do 
                                        writeTVar tpresence $ Interface nseqs projs ssamples
                                        return $ project sc proj
            players = atomically $ do 
                        Interface seqs projs ssamples <-  readTVar tpresence
                        return $ map (what %~ either scontrol2Playable ssamples2Playable) $ IM.elems ssamples
                
        t <- doesFileExist "current.medibox"
        when t $ do 
                v <- readFile  "current.medibox"
                atomically $ writeTVar tpresence $ read v
        
        forkIO $ outputCycle  (t0 + 1) serveProj (return $ Globals 0.1 4 100) players 
        forkIO . forever $ do 
                sleepThread 5
                v  <- atomically $ readTVar tpresence
                writeFile "current.medibox" $ show v
                
        gui msounds guiIn guiOut tpresence tselection

         

        
        --forkIO $ directControl readCInterface
        --outputCycle now readGlobals readInterface 
        


