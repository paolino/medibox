{- |
Play two precisely timed beats simultaneously
where the speed can be controlled by MIDI controllers.

Whenever the speed is changed we have to cancel
the events that are already scheduled.
So we use this example to demonstrate removing output events.
-}

{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}
module TestAlsa where

import Common (handleExceptionCont, )

import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port.InfoMonad as PortInfo
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event.RemoveMonad as Remove
import Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer.Queue as Queue
import qualified Sound.ALSA.Sequencer.RealTime as RealTime
import qualified Sound.ALSA.Sequencer.Time as Time
import qualified Sound.ALSA.Sequencer as SndSeq

import qualified System.Exit as Exit
import qualified System.IO as IO
import System.Environment (getArgs, )

import Control.Monad.Trans.Cont (ContT(ContT), )
import Control.Monad.IO.Class (liftIO, )
import Control.Monad (mplus,forM_,forever)
import Data.Maybe.HT (toMaybe, )
import Sound.OSC
import Board
import Control.Concurrent.STM
import Control.Concurrent
import qualified Data.Map as M
import Data.List



data N = N Int Int Time deriving (Show,Read)

data NE = Off Int |  On Int Int deriving (Eq,Ord, Show,Read)

convert :: E N  ->  [E NE]
convert (E (N p v dt) t) = [E (On p v) t,E (Off p) $ t + dt]


sequp :: TChan () -> TVar [E NE] -> TVar (M.Map Int Int) -> IO ()
sequp u tracks cmap = do 
  handleExceptionCont $ do
    h <- ContT $ SndSeq.withDefault SndSeq.Block
    liftIO $ Client.setName h "Haskell-Beat"
    (public :: Port.T)<-
       ContT $ Port.withSimple h "inout"
          (Port.caps [Port.capRead, Port.capSubsRead,
                      Port.capWrite, Port.capSubsWrite])
          (Port.types [Port.typeMidiGeneric, Port.typeApplication])
    -- a clever way to wait until last event has been sent
    private <-
       ContT $ Port.withSimple h "private"
          (Port.caps [Port.capRead, Port.capWrite])
          (Port.types [Port.typeMidiGeneric])
    -- the time accurate queue from alsa seq
    q <- ContT $ Queue.with h
    liftIO . forkIO $ controls u cmap h public
    liftIO $ mainIO tracks h public q



controls :: TChan () -> TVar (M.Map Int Int) -> SndSeq.T SndSeq.DuplexMode -> Port.T -> IO ()
controls u tm h public = forever $ do
  e <- Event.input h
  case Event.body e of
        CtrlEv Controller (Ctrl 
            (Channel (fromIntegral -> cha)) 
            (Parameter (fromIntegral -> par)) 
            (Value (fromIntegral -> val))
            ) -> atomically $ do 
                        modifyTVar tm $ M.insert par val 
                        writeTChan u ()
        _ -> return ()
  print e

mainIO :: TVar [E NE] -> SndSeq.T SndSeq.DuplexMode -> Port.T -> Queue.T -> IO ()
mainIO tt h public  q = do
  -- initialize for using time 
  PortInfo.modify h public $ do
     PortInfo.setTimestamping True
     PortInfo.setTimestampReal True
     PortInfo.setTimestampQueue q

  c <- Client.getId h

  let mkEv t e =
         (Event.simple (Addr.Cons c public) e) {
             Event.queue = q,
             Event.time = Time.consAbs . Time.Real . RealTime.fromDouble $ t
          }

  let  play t onoff p v =
         (Event.output h $ mkEv t $ Event.NoteEv onoff $
          Event.simpleNote (Event.Channel 0) p v)

  Queue.control h q Event.QueueStart Nothing

  let schedule n quant p = do
         let  period = p / fromIntegral quant
              l = n `mod` quant
              l' = (n - 2) `mod` quant
              t = fromIntegral n * period
         ons <- sort <$> pickBoard (Span (fromIntegral l * period) (fromIntegral (l + 1) * period)) <$> readTVarIO tt
         let  k (On p v) = play t Event.NoteOn (Event.Pitch $ fromIntegral p) (Event.Velocity $ fromIntegral v)
              k (Off p) = play t Event.NoteOff (Event.Pitch $ fromIntegral p) (Event.Velocity $ fromIntegral 0)
         forM_ ons k
         _ <- Event.drainOutput h
         return ()

  -- main loop
  t0 <- Sound.OSC.time
  let bar = 120 / 125
  let go n p = do
         sleepThreadUntil $ t0 + bar * fromIntegral n / fromIntegral p
         schedule (n + 1) p bar
         go (n + 1) p
  go 0 8
