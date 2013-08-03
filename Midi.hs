{-# LANGUAGE ViewPatterns #-}
module Midi where

import Control.Concurrent.STM
import Control.Concurrent
import Data.List 

import qualified Data.IntMap as IM
import Control.Arrow
import Control.Monad

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Exception as AlsaExc
import qualified Sound.ALSA.Sequencer.Connect as Connect


import System.IO 
import Control.Lens

import Sequenza 

-- interface must select a track or a parameter inside a hype or the hype as a track
data Selection = Tracks Int | Parameters Int deriving Show



regchan = 0 
-- send out the state of the selected track or parameter
midiOut :: TVar (IM.IntMap Sequenza)  -> TVar Selection -> TChan () -> IO ()
midiOut thypes top tp = (do
  SndSeq.withDefault SndSeq.Block $ \h -> do
        Client.setName (h :: SndSeq.T SndSeq.OutputMode) "waves"
        c <- Client.getId h
        Port.withSimple h "out" (Port.caps [Port.capRead, Port.capSubsRead]) Port.typeMidiGeneric $ \p -> forever $ do
                () <- atomically $ readTChan tp
                op <- atomically $ readTVar top
                hypes <- atomically $ readTVar thypes
                let send seqs ls las = case las of
                        Tracks prog ->  case IM.lookup prog seqs of
                                        Nothing -> return ()
                                        Just s -> do 
                                                forM_ (zip [0..] ls) $ \(par,ml) -> do
                                                        let ev =  Event.forConnection (Connect.toSubscribers (Addr.Cons c p)) $ 
                                                                        Event.CtrlEv Event.Controller (Event.Ctrl 
                                                                                (Event.Channel $ fromIntegral regchan) 
                                                                                (Event.Parameter $ fromIntegral par) 
                                                                                (Event.Value $ fromIntegral $ s ^. ml)
                                                                                )
                                                        void $ Event.outputDirect h $ ev 
                                                void $ Event.outputDirect h $  Event.forConnection (Connect.toSubscribers (Addr.Cons c p)) $ 
                                                                Event.CtrlEv Event.Controller (Event.Ctrl 
                                                                        (Event.Channel $ fromIntegral regchan) 
                                                                        (Event.Parameter $ fromIntegral 127) 
                                                                        (Event.Value $ fromIntegral $ prog)
                                                                        )
                                                void $ Event.outputDirect h $  Event.forConnection (Connect.toSubscribers (Addr.Cons c p)) $ 
                                                                Event.CtrlEv Event.Controller (Event.Ctrl 
                                                                        (Event.Channel $ fromIntegral regchan) 
                                                                        (Event.Parameter $ fromIntegral 117) 
                                                                        (Event.Value $ fromIntegral $ case s ^. pres of 
                                                                                Base _ -> 0
                                                                                _ -> 127)
                                                                        )
                                                

                        Parameters par ->  case par >= 29 of
                                        True -> return ()
                                        False -> do 
                                                forM_ (IM.assocs seqs) $ \(ctrl,prog) -> do
                                                        let ev =  Event.forConnection (Connect.toSubscribers (Addr.Cons c p)) $ 
                                                                        Event.CtrlEv Event.Controller (Event.Ctrl 
                                                                                (Event.Channel $ fromIntegral regchan) 
                                                                                (Event.Parameter $ fromIntegral ctrl) 
                                                                                (Event.Value $ fromIntegral $ prog ^. (ls !! par))
                                                                                )
                                                        void $ Event.outputDirect h $ ev 
                                                void $ Event.outputDirect h $  Event.forConnection (Connect.toSubscribers (Addr.Cons c p)) $ 
                                                                Event.CtrlEv Event.Controller (Event.Ctrl 
                                                                        (Event.Channel $ fromIntegral regchan) 
                                                                        (Event.Parameter $ fromIntegral 126) 
                                                                        (Event.Value $ fromIntegral $ par)
                                                                        )
                send hypes seq_lenses op
                ) `AlsaExc.catch` \e -> putStrLn $ "alsa_exception: " ++ AlsaExc.show e
ntracks = 24
session  = "current.sxn"

inside n m v  | v < n = n
              | v > m = m
              | True = v  

-- wait an event , modify the state or the selection and send an update in case
-- midiIn :: TVar (M.Map Int Sequenza) -> TVar Selection -> TChan () -> TVar (Double,Double,Int)


midiIn  thypes tselection tupdate tsupdate ttmp tdeps = (do
  tcopy <- newTVarIO 0
  SndSeq.withDefault SndSeq.Block $ \h -> do
        Client.setName (h :: SndSeq.T SndSeq.InputMode) "Ws"
        c <- Client.getId h
        Port.withSimple h "control in" (Port.caps [Port.capWrite, Port.capSubsWrite]) Port.typeMidiGeneric $ \p -> forever $ do
                ev <-  Event.input h
                case Event.body ev of
                     Event.CtrlEv Event.Controller (Event.Ctrl 
                                        (Event.Channel cha) 
                                        (Event.Parameter par) 
                                        (Event.Value val)
                                        ) -> do
                        let n = fromIntegral cha
                            v = fromIntegral val  
                            v' = v  
                            pa = fromIntegral par
                        zo <- atomically $ readTVar tselection
                        when  (n == regchan) $ do 
                                 case pa of 
                                        127 -> atomically $ when (v < ntracks) $ writeTVar tselection (Tracks v) >> writeTChan tupdate ()
                                        126 -> atomically $ when (v < 30) $ writeTVar tselection (Parameters v) >> writeTChan tupdate ()
                                        125 -> atomically $ modifyTVar ttmp $ \(n,sp,sh,ph) -> (n,n * fromIntegral v,sh,ph)
                                        124 -> atomically $ modifyTVar ttmp $ \(n,sp,sh,ph) -> (n,sp,(fromIntegral v / 128) * 15 / sp / n,ph)
                                        123 -> atomically $ modifyTVar ttmp $ \(n,sp,sh,ph) -> (n,sp,sh,v)
                                        118 -> atomically $ modifyTVar ttmp $ \(n,sp,sh,ph) -> (fromIntegral v,sp,sh,ph)
                                        122 -> readFile session >>= \(read -> (tm,ts,deps)) -> atomically $ do 
                                                writeTVar thypes ts
                                                writeTVar ttmp tm
                                                writeTVar tdeps deps
                                                writeTChan tupdate ()
                                                forM_ [0..ntracks - 1] $ writeTChan tsupdate 

                                        121 -> do
                                                ts <- atomically $ readTVar thypes
                                                tm <- atomically $ readTVar ttmp
                                                deps <- atomically $ readTVar tdeps
                                                writeFile session (show (tm,ts,deps))
                                        120 -> atomically $ case v of 
                                                        127 -> do 
                                                                op <- readTVar tselection 
                                                                case op of (Tracks l) -> writeTVar tcopy l
                                                                           _ -> return ()
                                                        _ -> return ()
                                        119 -> atomically $ case v of 
                                                        127 -> do 
                                                                op <- readTVar tselection 
                                                                case op of (Tracks l) -> do
                                                                                        v <- readTVar tcopy 
                                                                                        modifyTVar thypes $ \s -> (IM.insert l (s IM.! v) s)
                                                                                        writeTChan tupdate ()
                        
                                                                           _ -> return ()
                                                        _ -> return ()
                                        117 -> atomically $ case v of 
                                                        127 -> do 
                                                                op <- readTVar tselection 
                                                                case op of 
                                                                        Tracks l -> modifyTVar thypes $ \s -> case IM.lookup l s of
                                                                                Nothing -> s
                                                                                Just r -> case r ^. pres of 
                                                                                        Base m -> IM.insert l (pres .~ Higher m $ r) s
                                                                                        _ -> s
                                                                        _ -> return ()
                                                        0 -> do 
                                                                op <- readTVar tselection 
                                                                case op of 
                                                                        Tracks l -> modifyTVar thypes $ \s -> case IM.lookup l s of
                                                                                Nothing -> s
                                                                                Just r -> case r ^. pres of 
                                                                                        Higher m -> IM.insert l (pres .~ Base m $ r) s
                                                                                        _ -> s
                                                                        _ -> return ()
                                                        _ -> return ()
                                        _ ->   atomically $ do
                                                         
                                                        op <- readTVar tselection
                                                        case op of
                                                                Tracks prog -> 
                                                                        case pa of 
                                                                                 0 -> do
                                                                                        ds <- readTVar tdeps
                                                                                        r <- fmap ((^. pres) . (IM.! prog)) $ readTVar thypes
                                                                                        case r of
                                                                                                Higher _ -> do 
                                                                                                        let ds' = (prog,v) : filter ((/= prog) . fst) ds
                                                                                                        when (correct prog ds') $ do
                                                                                                                writeTVar tdeps ds' 
                                                                                                                modifyTVar thypes $ 
                                                                                                                        IM.adjust ((seq_lenses !! pa) .~ v) prog 
                                                                                                        writeTChan tsupdate prog

                                                                                                Base _ ->  do
                                                                                                                modifyTVar thypes $ 
                                                                                                                        IM.adjust ((seq_lenses !! pa) .~ v) prog 
                                                                                                                writeTChan tsupdate prog


                                                                                 _ -> do
                                                                                        when (pa > 0 && pa < 29) $ do
                                                                                                modifyTVar thypes $ IM.adjust ((seq_lenses !! pa) .~ v) prog 
                                                                                                writeTChan tsupdate prog
                                                                Parameters ctrl -> when (pa < ntracks) $ do
                                                                        modifyTVar thypes $ IM.adjust ((seq_lenses !! ctrl) .~ v) pa
                                                                        writeTChan tsupdate pa
                     _ -> return ()
                        
        ) `AlsaExc.catch` \e -> putStrLn $ "alsa_exception: " ++ AlsaExc.show e



