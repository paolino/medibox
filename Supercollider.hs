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
import Instr

withSC3n :: Int -> Connection UDP a -> IO a
withSC3n i = withTransport (openUDP "127.0.0.1" $ fromIntegral i)

servers = [57110 .. 57117]

data CtxSynths = CtxSynths (Int -> String) Int

ctxSynths :: TVar [Int] -> (Int -> String) -> IO CtxSynths
ctxSynths trr f = do 
        i <- atomically $ do
                (i:is) <- readTVar trr
                writeTVar trr is
                return i
        return (CtxSynths f i)
                

ck p z =  (in' 1 KR (control KR p z))
bootSample :: Int -> (Int,(FilePath,String)) -> IO ()
bootSample j (n,(fp,i)) = do
        withSC3n j . send $ b_free n
        withSC3n j . send $ b_allocRead n fp 0 0
        withSC3n j . send $ d_recv . synthdef i . out (control KR "outbus" 0) $ 
                        ck "p1" 0 * playBuf 2 AR (fromIntegral n) (0.5 + 1.5 * ck "p2" 0.5) 0 0 NoLoop RemoveSynth  

bootLPF :: Int -> (String,UGen -> UGen -> UGen)  -> IO ()
bootLPF j (i,fil) = do
        let     e_d =  envPerc (ck "p2" 0) (ck "p3" 0) 
                e = envGen AR 1 1 0 1 RemoveSynth e_d
        withSC3n j . send $ d_recv . synthdef i . out (control KR "outbus" 4) $ 
                         ck "p1" 0 * e * ( fil (in' 2 AR $ control KR "inbus" 0) $ 30 + (exp (9 * ck "p4" 0)))


initSynths :: FilePath -> IO (Realize Synth -> Playable)
initSynths sampledir = do
        putStrLn "Reading samples"
        sampledir <- getCurrentDirectory
        forM_ servers $ \i -> withSC3n i . send $ p_new [(1, AddToTail, 0)]
        ls <- map (id &&& takeBaseName)  `fmap` (find (depth ==? 0) (extension ==? ".wav") sampledir)
        sequence_ $ do 
                j <- servers
                l <- zip [0..] ls
                return $ bootSample j l
        sequence_ $ do 
                j <- servers
                l <- [("lpf1",lpf),("lpf2",lpf),("hpf1",hpf),("hpf2",hpf)]
                return $ bootLPF j l

        let     msamples = IM.fromList $ zip [0..] $ map snd ls ++ ["lpf1","lpf2","hpf1","hpf2"]
        mapM_ print $ IM.assocs msamples
        trr <- newTVarIO $ cycle servers
        let fs i = msamples IM.! (i `mod` IM.size msamples)
        return $ \s -> Playable (ctxSynths trr fs, s)

instance Play Synth  where
        type Ctx Synth  = CtxSynths
        play (CtxSynths f i) e@(t, (_,Synth sa inb out p1 p2 p3 p4 p5 p6)) = do
                print e
                withSC3n i . sendBundle . bundle t . return $ 
                         s_new (f sa) (-1) AddToHead 1 $ [("inbus", fromIntegral inb), ("outbus",fromIntegral out)
                                ,("p1",from128 p1),("p2",from128 p2),("p3",from128 p3)
                                , ("p4",from128 p4),("p5",from128 p5),("p6",from128 p6)]

instance Play SControl where
        type Ctx SControl = ()
        play () e@(t,(vo,SControl bus)) = do 
                print e
                forM_ servers $ \i -> withSC3n i . sendBundle . bundle t . return $ c_set [(bus,vo)]
  
              
privates = numOutputBuses + numInputBuses
{-
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
-}              
