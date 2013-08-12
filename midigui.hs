{-# LANGUAGE ScopedTypeVariables #-}
import Graphics.UI.Gtk
import Control.Monad.Trans
import Control.Monad

import qualified Data.IntMap as M
import Control.Concurrent
import System.IO


import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Exception as AlsaExc
import Sound.OSC
import System.Environment (getArgs, )


import qualified Sound.ALSA.Sequencer.Connect as Connect

import qualified System.Exit as Exit
import qualified System.IO as IO
import Control.Concurrent.STM

midiIn ::TChan (Int,Int)  -> IO ()
midiIn  toutstate = (do
  -- tcopy <- newTVarIO zero
  SndSeq.withDefault SndSeq.Block $ \h -> do
        Client.setName (h :: SndSeq.T SndSeq.InputMode) "Midi Controller xxx"
        c <- Client.getId h
        Port.withSimple h "in" (Port.caps [Port.capWrite, Port.capSubsWrite]) Port.typeMidiGeneric $ \p -> forever $ do
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
                        when  (n == 0) $ do 
                                atomically $ writeTChan toutstate (pa,v)
                     _ -> return ()           
                
        )
                `AlsaExc.catch` \e -> putStrLn $ "alsa_exception: " ++ AlsaExc.show e



midiOut :: TChan (Int,Int,Int) -> IO ()
midiOut ech = (do
  SndSeq.withDefault SndSeq.Block $ \h -> do
        Client.setName (h :: SndSeq.T SndSeq.OutputMode) "Midi Controller xxx"
        c <- Client.getId h
        Port.withSimple h "out" (Port.caps [Port.capRead, Port.capSubsRead]) Port.typeMidiGeneric $ \p -> forever $ do
                (cha,par,val) <- atomically $ readTChan ech
                let ev =  Event.forConnection (Connect.toSubscribers (Addr.Cons c p)) $ 
                                Event.CtrlEv Event.Controller (Event.Ctrl 
                                        (Event.Channel $ fromIntegral cha) 
                                        (Event.Parameter $ fromIntegral par) 
                                        (Event.Value $ fromIntegral val)
                                        )
                void $ Event.outputDirect h $ ev 
        )
                `AlsaExc.catch` \e -> putStrLn $ "alsa_exception: " ++ AlsaExc.show e

hello :: (ButtonClass o) => o -> IO ()
hello b = set b [buttonLabel := "Hello World"]

myAddSpinButton :: Int -> HBox -> Int -> Int -> IO Label
myAddSpinButton k box min max = do
    label <- labelNew (Just $ show k)
    labele <- eventBoxNew
    set labele [containerChild:= label]
    widgetSetSizeRequest labele 40 10
    frame <- frameNew
    set frame [containerChild:= labele]
    boxPackStart box frame PackNatural 0
    on labele scrollEvent $  tryEvent $ do 
        ScrollUp <- eventScrollDirection
        liftIO $ do 
                x <- fmap read $ labelGetText label
                let y = limitedAdd max 1 x
                labelSetText label $ show y
    on labele scrollEvent $  tryEvent $ do 
        ScrollDown <- eventScrollDirection
        liftIO $ do 
                x <- fmap read $ labelGetText label
                let y = limitedSubtract min 1 x
                labelSetText label $ show y
    return label


                 
name thandle save load hbox =   do
                label <- labelNew $ Just "Control"
                labelSetJustify label JustifyLeft
                name <- buttonNew
                set name [containerChild := label]
                widgetSetSizeRequest name 200 10
                boxPackStart hbox name PackNatural 0
                e <- entryNew
                entrySetHasFrame e False
                widgetSetNoShowAll e True
                boxPackStart hbox e PackNatural 0
                name `on` buttonActivated $ do
                        set e [widgetVisible := True]
                        set name [widgetVisible := False]
                        widgetGrabFocus e
                e `on` entryActivate $ do 
                        t <- entryGetText e
                        set e [widgetVisible := False]
                        buttonSetLabel name t
                        set name [widgetVisible := True]
                save `on` buttonActivated $ do
                        l <- buttonGetLabel name
                        mh <- atomically $ readTVar thandle
                        case mh of 
                                Nothing -> return ()
                                Just h -> hPutStrLn h l
                load `on` buttonActivated $ do
                        mh <- atomically $ readTVar thandle
                        case mh of 
                                        Nothing -> return ()
                                        Just h -> do
                                                l <- hGetLine h
                                                buttonSetLabel name $ l


                
                
m = 0
k = 1/128
main :: IO ()
main = do
  toutstate <- newTChanIO
  ech <- newTChanIO
  thandle <- newTVarIO Nothing


  forkIO $ midiOut ech
  initGUI
  forkIO $ midiIn toutstate  
  window <- windowNew

   
  ad <- adjustmentNew 0 0 400 1 10 400
  sw <- scrolledWindowNew Nothing (Just ad)
  set window [windowDefaultWidth := 200, windowDefaultHeight := 200,
                          containerBorderWidth := 10, containerChild := sw]

  vbox    <- vBoxNew True 1
  scrolledWindowAddWithViewport sw vbox
  coms <- hBoxNew False 1
  boxPackStart vbox coms PackNatural 0
  res <- buttonNewWithLabel "Sync"
  boxPackStart coms res PackNatural 0
  load <- buttonNewFromStock "Load"
  boxPackStart coms load PackNatural 0
  fc <- fileChooserButtonNew "Select a file" FileChooserActionOpen
  boxPackStart coms fc PackGrow 0
  save <- buttonNewFromStock "Save"
  boxPackStart coms save PackNatural 0
  quit <- buttonNewFromStock "Quit"
  boxPackStart coms quit PackNatural 0
   
  quit `on` buttonActivated $ mainQuit
  save `on` buttonActivated $ do
        mname <- fileChooserGetFilename fc
        case mname of 
                Nothing -> return ()
                Just name -> do
                        h <- openFile name WriteMode 
                        atomically $ writeTVar thandle (Just h)
  load `on` buttonActivated $ do
        mname <- fileChooserGetFilename fc
        case mname of 
                Nothing -> return ()
                Just name -> do
                        h <- openFile name ReadMode 
                        atomically $ writeTVar thandle (Just h)
  forM_ [0..127] $ \n -> do 
          hbox    <- hBoxNew False 1
          widgetSetSizeRequest hbox 600 15
          boxPackStart vbox hbox PackGrow 0
          name thandle save load hbox 
          label <- labelNew (Just "0")
          widgetSetSizeRequest label 40 10
          frame <- frameNew
          set frame [containerChild:= label]
          param <- myAddSpinButton n hbox  0 127 
          boxPackStart hbox frame PackNatural 0
          eb <- eventBoxNew
          button2 <- progressBarNew 
          set eb [containerChild:= button2]
          
          widgetAddEvents eb [Button1MotionMask]
          boxPackStart hbox eb PackGrow 0
          progressBarSetFraction button2 0
          memory <- labelNew (Just $ show (0,0))
          save `on` buttonActivated $ do
                        x <- progressBarGetFraction button2 
                        paramv <- fmap read $ labelGetText param
                        mh <- atomically $ readTVar thandle
                        case mh of 
                                Nothing -> return ()
                                Just h -> hPutStrLn h $ show (paramv::Int,floor $ x/k :: Int)
          load `on` buttonActivated $ do
                        mh <- atomically $ readTVar thandle
                        case mh of 
                                Nothing -> return ()
                                Just h -> do
                                        l <- hGetLine h
                                        let (paramv::Int,wx :: Int) = read l      
                                        progressBarSetFraction button2 (fromIntegral wx * k)
                                        labelSetText label $ show wx
                                        labelSetText param $ show paramv
          dupchan <- atomically $ dupTChan toutstate
          forkIO . forever $ do
                         
                        (tp,wx) <- atomically $ readTChan dupchan
                        postGUIAsync $ do 
                                paramv <- read `fmap` (labelGetText param)
                                wx' <- read `fmap` (labelGetText label)
                                case tp == paramv && wx' /= wx of
                                        False -> return ()
                                        True -> do 
                                                progressBarSetFraction button2 (fromIntegral wx * k)
                                                labelSetText label $ show wx


          res `on` buttonActivated $ do
                        x <- progressBarGetFraction button2 
                        paramv <- fmap read $ labelGetText param
                        atomically $ writeTChan ech (m,paramv,floor $ x/k)
          on eb motionNotifyEvent $ do 
                (r,_) <- eventCoordinates
                (r',0) <- liftIO $ fmap read $ labelGetText memory
                liftIO $ do 
                        x <- progressBarGetFraction button2 
                        let y = (if r > r' then limitedAdd 1 else limitedSubtract 0) k x
                        progressBarSetFraction button2 y
                        paramv <- fmap read $ labelGetText param
                        atomically $ writeTChan ech (m,paramv,floor $ y/k)
                        labelSetText label $ show (floor $ y/k)
                        labelSetText memory $ show (r,0)
                return True
          on button2 scrollEvent $  tryEvent $ do 
                ScrollUp <- eventScrollDirection
                liftIO $ do 
                        x <- progressBarGetFraction button2 
                        let y = limitedAdd 1 k x
                        paramv <- fmap read $ labelGetText param
                        atomically $ writeTChan ech (m,paramv,floor $ y/k)
                        progressBarSetFraction button2 y
                        labelSetText label $ show (floor $ y/k)
          on button2 scrollEvent $  tryEvent $ do 
                ScrollDown <- eventScrollDirection
                liftIO $ do 
                        x <- progressBarGetFraction button2 
                        let y = limitedSubtract 0 k x
                        paramv <- fmap read $ labelGetText param
                        atomically $ writeTChan ech (m,paramv,floor $ y/k)
                        progressBarSetFraction button2 y
                        labelSetText label $ show (floor $ y/k)
  save `on` buttonActivated $ do
        mh <- atomically $ readTVar thandle
        case mh of 
                Nothing -> return ()
                Just h -> hClose h
  load `on` buttonActivated $ do
        mh <- atomically $ readTVar thandle
        case mh of 
                Nothing -> return ()
                Just h -> hClose h

  onDestroy window mainQuit
  widgetShowAll window
  mainGUI

limitedAdd xm d x
        | x + d > xm = xm
        | otherwise = x + d
limitedSubtract xm d x
        | x - d < xm = xm
        | otherwise = x - d
