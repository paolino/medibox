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
-- import Supercollider
import Midi2
import Haskell
-- import Realize
import Dynamic
delay = 0.01
sampledir = "."
    
weight = 127 
main = do 
        tpresence <- newTVarIO $ IM.fromList $ zip [0..127] $ repeat (Base zero, Nothing)
        bootMidi tpresence undefined undefined undefined undefined
        forever $ do 
                sleepThread 1 
                pr <- atomically $ readTVar tpresence
                let Just (_,sc,_) = querySequenza pr 0 
                print sc
        --forkIO $ directControl readCInterface
        --outputCycle now readGlobals readInterface 
        


