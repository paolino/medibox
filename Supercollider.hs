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




servers = [57110,57111 .. 57117]

initSamples :: FilePath -> IO (Int -> Double -> Double -> Double -> IO ())
initSamples sampledir = do
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
        return (\i -> sound trr (msamples IM.! ((i `mod` length ls)))) 


sound :: TVar [Int] -> String -> Double -> Double -> Double -> IO ()
sound trr s mt amp pitch = do
        i <- atomically $ do
                (i:is) <- readTVar trr
                writeTVar trr is
                return i
        print (i,s,mt,amp,pitch)
        withSC3n i . sendBundle . bundle mt $ [s_new s (-1) AddToTail 1 $ [("amp",amp),("rate",pitch)]]
        
bootSample :: Int -> (Int,(FilePath,String)) -> IO ()
bootSample j (n,(fp,i)) = do
        withSC3n j . send $ b_free n
        withSC3n j . send $ b_allocRead n fp 0 0
        withSC3n j . send $ d_recv . synthdef i . out 0 $ 
                        control KR "amp" 1 * playBuf 2 AR (fromIntegral n) (control KR "rate" 1) 0 0 NoLoop RemoveSynth  

{-
        forkIO $ forever $ runInputT  (defaultSettings{ historyFile = Just "waves.hist", autoAddHistory = True}) $ do
                        p <- liftIO $ atomically (readTVar tmp) 
                        ts <- liftIO $ atomically $ readTVar ttracks
                        o <- getInputLineWithInitial ":>" ("","")
                        case o of 
                                Nothing -> return ()
                                Just o -> do 
                                        let (p,ts) = read o
                                        liftIO $ atomically $ writeTVar tmp p
                                        liftIO $ atomically $ writeTVar ttracks (IM.fromList $ zip [1..] ts)
-}
withSC3n :: Int -> Connection UDP a -> IO a
withSC3n i = withTransport (openUDP "127.0.0.1" $ fromIntegral i)
