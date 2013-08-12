{-# LANGUAGE ViewPatterns #-}
module Midi2 where

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

import Dynamic (query, Pointing (..), core)
import Sequenza 
import PersistentChoiceLens

type Mem a = TVar (IM.IntMap a)


regchan = 0 

-- send out the state of the selected track or parameter
midiOut :: TVar DSequenze  ->  TVar (PersistentChoice PInt) -> TChan () -> IO ()
midiOut  tpresence  top tp = (do
  SndSeq.withDefault SndSeq.Block $ \h -> do
        Client.setName (h :: SndSeq.T SndSeq.OutputMode) "waves"
        c <- Client.getId h
        Port.withSimple h "out" (Port.caps [Port.capRead, Port.capSubsRead]) Port.typeMidiGeneric $ \p -> forever $ do
                -- wait
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
                                        Just (ps,_,_) -> do
                                                forM_ (zip [0..23] sequenza_lenses) $ \(par,ml) -> do
                                                        let ev =  Event.forConnection (Connect.toSubscribers (Addr.Cons c p)) $ 
                                                                        Event.CtrlEv Event.Controller (Event.Ctrl 
                                                                                (Event.Channel $ fromIntegral regchan) 
                                                                                (Event.Parameter $ fromIntegral par) 
                                                                                (Event.Value $ fromIntegral $ ps ^. core . ml)
                                                                                )
                                                        void $ Event.outputDirect h $ ev 
                                                void $ Event.outputDirect h $  Event.forConnection (Connect.toSubscribers (Addr.Cons c p)) $ 
                                                                Event.CtrlEv Event.Controller (Event.Ctrl 
                                                                        (Event.Channel $ fromIntegral regchan) 
                                                                        (Event.Parameter $ fromIntegral 117) 
                                                                        (Event.Value $ fromIntegral $ case ps of 
                                                                                Base _ -> 0
                                                                                _ -> 127)
                                                                        )
                                                
{-
                        B prog -> do
                                seqs <- atomically $ readTVar tssample
                                case IM.lookup (fromIntegral prog) seqs of
                                        Nothing -> return ()
                                        Just s -> do 
                                                forM_ (zip [0..23] ssample_lenses) $ \(par,ml) -> do
                                                        let ev =  Event.forConnection (Connect.toSubscribers (Addr.Cons c p)) $ 
                                                                        Event.CtrlEv Event.Controller (Event.Ctrl 
                                                                                (Event.Channel $ fromIntegral regchan) 
                                                                                (Event.Parameter $ fromIntegral par) 
                                                                                (Event.Value $ fromIntegral $ s ^. ml)
                                                                                )
                                                        void $ Event.outputDirect h $ ev 
                        C prog -> do
                                seqs <- atomically $ readTVar tscontrollo
                                case IM.lookup (fromIntegral prog) seqs of
                                        Nothing -> return ()
                                        Just s -> do 
                                                forM_ (zip [0..23] scontrollo_lenses) $ \(par,ml) -> do
                                                        let ev =  Event.forConnection (Connect.toSubscribers (Addr.Cons c p)) $ 
                                                                        Event.CtrlEv Event.Controller (Event.Ctrl 
                                                                                (Event.Channel $ fromIntegral regchan) 
                                                                                (Event.Parameter $ fromIntegral par) 
                                                                                (Event.Value $ fromIntegral $ s ^. ml)
                                                                                )
                                                        void $ Event.outputDirect h $ ev 
                                                
                        D prog -> do
                                s <- atomically (rq prog)
                                forM_ (zip [0..1] sbus_lenses) $ \(par,ml) -> do
                                                        let ev =  Event.forConnection (Connect.toSubscribers (Addr.Cons c p)) $ 
                                                                        Event.CtrlEv Event.Controller (Event.Ctrl 
                                                                                (Event.Channel $ fromIntegral regchan) 
                                                                                (Event.Parameter $ fromIntegral par) 
                                                                                (Event.Value $ fromIntegral $ s ^. ml)
                                                                                )
                                                        void $ Event.outputDirect h $ ev 
                -}                
                ) `AlsaExc.catch` \e -> putStrLn $ "alsa_exception: " ++ AlsaExc.show e

session  = "current.sxn"

-- wait an event , modify the state or the selection and send an update in case
-- midiIn :: TVar (M.Map Int Sequenza) -> TVar Selection -> TChan () -> TVar (Double,Double,Int)
bootMidi tpresence tssample tscontrollo ttmp qu = do
        tdeps <- newTVarIO []
        tupdate <- newTChanIO 
        tselection <- newTVarIO zero :: IO (TVar (PersistentChoice PInt))
        forkOS $ midiIn tpresence tselection tupdate        
        forkOS $ midiOut  tpresence tselection tupdate 

midiIn ::TVar DSequenze -> TVar (PersistentChoice PInt) -> TChan () -> IO ()
midiIn  tpresence  tselection tupdate  = (do
  -- tcopy <- newTVarIO zero
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
                            pa = fromIntegral par
                        when  (n == regchan) $ do 
                                 print (n,v,pa)
                                 case pa of 
                                        127 -> atomically $ do 
                                                modifyTVar tselection $ (choice .~ PInt v)
                                        126 -> atomically $ modifyTVar tselection nextChoice
                                        {-
                                        125 -> atomically $ modifyTVar ttmp $ \(n,sp,sh,ph) -> (n,n * fromIntegral v,sh,ph)
                                        124 -> atomically $ modifyTVar ttmp $ \(n,sp,sh,ph) -> (n,sp,(fromIntegral v / 128) * 15 / sp / n,ph)
                                        123 -> atomically $ modifyTVar ttmp $ \(n,sp,sh,ph) -> (n,sp,sh,v)
                                        122 -> atomically $ modifyTVar ttmp $ \(n,sp,sh,ph) -> (fromIntegral v,sp,sh,ph)
                                        121 -> readFile session >>= \(read -> (pres,samples,control,deps)) -> atomically $ do 
                                                writeTVar tpresence pres
                                                writeTVar tssample samples
                                                writeTVar tscontrollo control
                                                writeTVar tdeps deps
                                                --forM_ [0..ntracks - 1] $ writeTChan tsupdate 

                                        120 -> do
                                                pres <- atomically $ readTVar tpresence
                                                samples <- atomically $ readTVar tssample
                                                control <- atomically $ readTVar tscontrollo
                                                deps <- atomically $ readTVar tdeps
                                                writeFile session (show (pres,samples,control,deps))
                                        
                                        119 -> atomically $ case v of 
                                                        127 -> do 
                                                                op <- readTVar tselection 
                                                                writeTVar tcopy op
                                                        0 -> do 
                                                                op <- readTVar tselection 
                                                                v <- fmap (fromIntegral . view choice) $ readTVar tcopy 
                                                                case op ^. persistentChoice of 
                                                                           (A (PInt l)) -> modifyTVar tpresence $ \s -> (IM.insert l (s IM.! v ) s)
                                                                           (B (PInt l)) -> modifyTVar tssample $ \s -> (IM.insert l (s IM.! v) s)
                                                                           (C (PInt l)) -> modifyTVar tscontrollo $ \s -> (IM.insert l (s IM.! v) s)
                                                        _ -> return ()

                                        117 -> atomically $ case v of 
                                                        127 -> do 
                                                                op <- readTVar tselection 
                                                                case op ^. persistentChoice of 
                                                                        A (PInt l) -> modifyTVar tpresence $ \s -> case IM.lookup l s of
                                                                                Just (Base m) -> IM.insert l (Higher m) s
                                                                                _ -> s
                                                                        _ -> return ()
                                                        0 -> do 
                                                                op <- readTVar tselection 
                                                                case op ^. persistentChoice of 
                                                                        A (PInt l) -> modifyTVar tpresence $ \s -> case IM.lookup l s of
                                                                                Just (Higher m) -> IM.insert l (Base m) s
                                                                                _ -> s
                                                                        _ -> return ()
                                                        _ -> return ()
                                        -}
                                        _ ->   atomically $ do
                                                        op <- readTVar tselection
                                                        case op ^. persistentChoice of
                                                                A (PInt prog) -> do
                                                                        seqs <- readTVar tpresence
                                                                        case querySequenza seqs (fromIntegral prog) of
                                                                                Nothing -> return ()
                                                                                Just (ps,_,_) -> do 
                                                                                        when (pa < 24) $ let 
                                                                                                mseqs = insertSequenza prog 
                                                                                                        ((core . (sequenza_lenses !! pa) .~  v) ps)
                                                                                                        seqs
                                                                                                in
                                                                                                case mseqs of   
                                                                                                        Nothing -> return ()
                                                                                                        Just seqs -> writeTVar tpresence seqs
                                                                                        when (pa == 24) $ let
                                                                                                setBase (Pointing x) =  Base x
                                                                                                setBase x = x
                                                                                                setPointing (Base x) = Pointing x
                                                                                                setPointing x = x
                                                                                                mseqs = flip (insertSequenza prog) seqs $ (if v == 0 then setBase else setPointing) $ ps
                                                                                                in
                                                                                                case mseqs of   
                                                                                                        Nothing -> return ()
                                                                                                        Just seqs -> writeTVar tpresence seqs

                                                                _ -> return ()
--                                                                B (PInt prog) -> modifyTVar tssample $ IM.adjust ((ssample_lenses !! pa) .~ v) $  prog 
--                                                                C (PInt prog) ->  modifyTVar tscontrollo $ IM.adjust ((scontrollo_lenses !! pa) .~ v) $ prog 
 --                                                               D (PInt prog) ->  sq (prog,((scontrollo_lenses !! pa) .~ v))
                                        
                                 op <- atomically $ readTVar tselection
                                 case op ^. persistentChoice of
                                        A (PInt prog) -> atomically (fmap ((IM.! prog)) (readTVar tpresence)) >>= print
--                                        B (PInt prog) -> atomically (fmap ((IM.! prog)) (readTVar tssample)) >>= print
--                                        C (PInt prog) -> atomically (fmap ((IM.! prog)) (readTVar tscontrollo)) >>= print
                                 atomically $ writeTChan tupdate ()
                     _ -> return ()
                        
        ) `AlsaExc.catch` \e -> putStrLn $ "alsa_exception: " ++ AlsaExc.show e



