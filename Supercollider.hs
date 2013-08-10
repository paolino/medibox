{-# LANGUAGE TypeFamilies, FlexibleInstances, NoMonomorphismRestriction #-}

module Supercollider where

import Control.Arrow ((&&&))
import qualified Data.IntMap as IM
import Control.Concurrent.STM
import Control.Monad (forM_)

import Sound.OSC
import Sound.SC3 hiding (pitch)
import System.FilePath
import System.FilePath.Find
import System.Directory

import Realize
import Sequenza

withSC3n :: Int -> Connection UDP a -> IO a
withSC3n i = withTransport (openUDP "127.0.0.1" $ fromIntegral i)

servers = [57110]

data CtxSamples = CtxSamples (Int -> String) Int

ctxSamples :: TVar [Int] -> (Int -> String) -> IO CtxSamples
ctxSamples trr f = do 
        i <- atomically $ do
                (i:is) <- readTVar trr
                writeTVar trr is
                return i
        return (CtxSamples f i)
                
bootSample :: Int -> (Int,(FilePath,String)) -> IO ()
bootSample j (n,(fp,i)) = do
        withSC3n j . send $ b_free n
        withSC3n j . send $ b_allocRead n fp 0 0
        withSC3n j . send $ d_recv . synthdef i . out (privates + control KR "outbus" 0) $ 
                        control KR "amp" 1 * playBuf 2 AR (fromIntegral n) (control KR "rate" 1) 0 0 NoLoop RemoveSynth  
initSamples :: FilePath -> IO (SSample Projection -> Playable)
initSamples sampledir = do
        putStrLn "Reading samples"
        sampledir <- getCurrentDirectory
        forM_ servers $ \i -> withSC3n i . send $ p_new [(1, AddToTail, 0)]
        ls <- map (id &&& takeBaseName)  `fmap` (find (depth ==? 0) (extension ==? ".wav") sampledir)
        
        sequence_ $ do 
                j <- servers
                l <- zip [0..] ls
                return $ bootSample j l
        let     msamples = IM.fromList $ zip [0..] $ map snd ls
        mapM_ print $ IM.assocs msamples
        trr <- newTVarIO $ cycle servers
        let fs i = msamples IM.! (i `mod` length ls)
        return $ \s -> Playable (ctxSamples trr fs, s)

instance Play (SSample Double) where
        type Ctx (SSample Double) = CtxSamples
        play (CtxSamples f i) e@(Event t (SSample vo pi sa out)) = do
                print e
                withSC3n i . sendBundle . bundle t $ [s_new (f sa) (-1) AddToHead 1 $ [("amp",vo),("rate",pi),("outbus",fromIntegral out)]]

                
privates = numOutputBuses + numInputBuses

filters  = 
        [       ("lpf1" , \input -> lpf input $ 30 + 10000 *(control KR "p1" 1))
      
        ,        ("lpf2" , \input -> lpf input $ 30 + 10000 *(control KR "p1" 1))
        ,        ("lpf3" , \input -> lpf input $ 30 + 10000 *(control KR "p1" 1))
        ,        ("lpf4" , \input -> lpf input $ 30 + 10000 *(control KR "p1" 1))
        ,        ("hpf1" , \input -> hpf input $ 1000 + 20000 *(control KR "p1" 0))
        ,        ("hpf2" , \input -> hpf input $ 1000 + 20000 *(control KR "p1" 0))
        ,       ("hpf3" , \input -> hpf input $ 1000 + 20000 *(control KR "p1" 0))
        ,        ("hpf4" , \input -> hpf input $ 1000 + 20000 *(control KR "p1" 0))
        ]

paras n = ["p1","p2","p3","p4","p5","p6","p7","p8"] !! (n `mod` 8)

data CtxFilters = CtxFilters ()
initFilters = do 
        sequence_ $ do 
                j <- servers
                (i,(n,d)) <- zip [1000..] filters
                return $ do 
                        print (i,n)
                        withSC3n j . send $ d_recv $ synthdef n $ out (privates + control KR "outbus" 4) $ d (in' 2 AR $ privates + (control KR "inbus" 0))
                
                        sleepThread 0.1
                        withSC3n j . send $ n_free [i] 
                        withSC3n j . send $ s_new n i AddToTail 1 []    
        sequence_ $ do 
                j <- servers
                -- identity filter, from 0 private to 0 hardware
                return $ do 
                        withSC3n j . send $ d_recv $ synthdef "ouput" $ out 0 $ (in' 2 AR $ privates)
                        sleepThread 0.1
                        withSC3n j . send $ s_new "ouput" (-1) AddToTail 1 []    
                        
        
        return $ \s -> Playable (return $ CtxFilters (), s)

instance Play (SControllo Double) where
        type Ctx (SControllo Double) = CtxFilters 
        play _ e@(Event t (SControllo fi pa x)) = do
                print e
                forM_ servers $ \j -> 
                        withSC3n j . sendBundle . bundle t $ [n_set (fi + 1000) $ [(paras pa,x)]]

instance Direct SBus where
        type DCtx SBus = Maybe Int
        direct (Just f) (SBus i o) = 
                forM_ servers $ \j -> 
                        withSC3n j . send  $ (n_set (f + 1000) $ [("inbus",fromIntegral i),("outbus",fromIntegral o)])
        direct Nothing (SBus i o) = return ()
                
