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
import Haskell
import Realize

delay = 0.01
sampledir = "."
    
weight = 127 
main = do 
        playable <- initSamples  sampledir
        (fplayable,nfilters) <- initFilters
        ttmp <- newTVarIO $ (1,128,0,0)
        let 
                patterns = zip [0..weight] $ repeat (Base basePresence)
                ssamples = zip [0..weight] $ repeat baseSSample
                scontrolli = zip [0..weight] $ repeat baseSControllo
        tpresence <- newTVarIO $ IM.fromList patterns
        tssample <- newTVarIO $ IM.fromList ssamples
        tscontrollo <- newTVarIO $ IM.fromList scontrolli
        qbus@(Query _ _ qq) <- atomically intmapQ  
        bootMidi tpresence tssample tscontrollo ttmp qbus
        let     fw = 256 -- fire window
                w = 4 -- pattern cycle
        let     readGlobals = return (Globals 0.01 4 256)
                readPInterface = do
                        (pres,samples,filters) <- atomically $ do 
                                pres <- readTVar tpresence
                                samples <- readTVar tssample
                                controlli <- readTVar tscontrollo
                                return (pres,samples,controlli)
                        let     ps = map playable (IM.elems samples) ++ map fplayable (IM.elems filters)
                                
                        return (pres, ps)
                readCInterface = atomically  qq
                        
        
        now <- time 
        forkIO $ directControl readCInterface
        outputCycle now readGlobals readInterface 
        


