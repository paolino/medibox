{-# LANGUAGE ViewPatterns #-}
module Interface where

import Control.Concurrent.STM
import Control.Concurrent

import Data.List 

import qualified Data.IntMap as IM
import Control.Arrow
import Control.Monad


import System.IO 
import Control.Lens

import Dynamic (query, Pointing (..), core)
import Sequenza 
import PersistentChoiceLens
-- type Mem a = TVar (IM.IntMap a)

session  = "current.sxn"

-- wait an event , modify the state or the selection and send an update in case
-- midiIn :: TVar (M.Map Int Sequenza) -> TVar Selection -> TChan () -> TVar (Double,Double,Int)
interface input output tpresence  = do
        tupdate <- newTChanIO 
        tselection <- newTVarIO zero :: IO (TVar (PersistentChoice PInt))
        t1 <- forkIO . forever $ report output tpresence tselection tupdate        
        t2 <- forkIO . forever $ interface input  tpresence tselection tupdate 
        return (killThread t1 >> killThread t2)

-- send out the state of the selected track or parameter
report :: TChan (Int,Int) -> TVar DSequenze  ->  TVar (PersistentChoice PInt) -> TChan () -> IO ()
report midiout  tpresence  top tp = do 
        () <- atomically $ readTChan tp
        -- see what interface is selected
        op <- atomically $ readTVar top
        print (op ^. persistentChoice)
        case op ^. persistentChoice of
                -- patterns are selected
                A prog -> do
                        seqs <- atomically $ readTVar tpresence
                        case querySequenza seqs (fromIntegral prog) of
                                Nothing -> return ()
                                Just (ps,_,_) -> atomically $ do
                                        forM_ (zip [0..23] sequenza_lenses) $ \(par,ml) -> writeTChan midiout (par, ps ^. core . ml)
                                        writeTChan midiout (24, case ps of 
                                                Base _ -> 0
                                                _ -> 127)
                                        


interface:: TChan (Int,Int) -> TVar DSequenze -> TVar (PersistentChoice PInt) -> TChan () -> IO ()
interface  midiin tpresence  tselection tupdate  = do
        (par,val) <- atomically $ readTChan midiin
        case par of 
                127 ->  atomically $ do 
                                modifyTVar tselection $ (choice .~ PInt val)
                                writeTChan tupdate ()
                126 -> atomically $ do
                                modifyTVar tselection nextChoice
                                writeTChan tupdate ()
                _ ->   atomically $ do
                        op <- readTVar tselection
                        case op ^. persistentChoice of
                                A (PInt prog) -> do
                                        seqs <- readTVar tpresence
                                        case querySequenza seqs (fromIntegral prog) of
                                                Nothing -> return ()
                                                Just (ps,_,_) -> do 
                                                        when (par < 24) $ let 
                                                                mseqs = insertSequenza prog 
                                                                        ((core . (sequenza_lenses !! par) .~  val) ps)
                                                                        seqs
                                                                in
                                                                case mseqs of   
                                                                        Nothing -> return ()
                                                                        Just seqs -> writeTVar tpresence seqs
                                                        when (par == 24) $ let
                                                                setBase (Pointing x) =  Base x
                                                                setBase x = x
                                                                setPointing (Base x) = Pointing x
                                                                setPointing x = x
                                                                mseqs = flip (insertSequenza prog) seqs $ (if val == 0 then setBase else setPointing) $ ps
                                                                in
                                                                case mseqs of   
                                                                        Nothing -> return ()
                                                                        Just seqs -> writeTVar tpresence seqs

                                _ -> return ()
        
        op <- atomically $ readTVar tselection
        case op ^. persistentChoice of
                A (PInt prog) -> atomically (fmap ((IM.! prog)) (readTVar tpresence)) >>= print
                        



