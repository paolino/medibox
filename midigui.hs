{-# LANGUAGE ScopedTypeVariables #-}
module GUI where

import Graphics.UI.Gtk
import Control.Monad.Trans
import Control.Monad

import qualified Data.IntMap as M
import Control.Concurrent
import System.IO


import Sound.OSC

import Control.Concurrent.STM
import MidiComm

                
midichannel = 0
k = 1/128
gui :: IO ()
gui = do
  (midiinchan, midioutchan, _) <- midiInOut "midi control GUI" midichannel 
  thandle <- newTVarIO Nothing


  initGUI
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
  forM_ [0..127] $ \paramv -> do 
          hbox    <- hBoxNew False 1
          widgetSetSizeRequest hbox 600 12
          boxPackStart vbox hbox PackGrow 0

          param <- labelNew (Just $ show paramv)
          widgetSetSizeRequest param 40 10
          frame <- frameNew
          set frame [containerChild:= param]
          boxPackStart hbox frame PackNatural 0

          label <- labelNew (Just "0")
          widgetSetSizeRequest label 40 10
          frame <- frameNew
          set frame [containerChild:= label]
          boxPackStart hbox frame PackNatural 0

          eb <- eventBoxNew
          level <- progressBarNew 
          set eb [containerChild:= level]
          
          widgetAddEvents eb [Button1MotionMask]
          boxPackStart hbox eb PackGrow 0
          progressBarSetFraction level 0
          memory <- labelNew (Just $ show (0,0))
          save `on` buttonActivated $ do
                        x <- progressBarGetFraction level 
                        mh <- atomically $ readTVar thandle
                        case mh of 
                                Nothing -> return ()
                                Just h -> hPutStrLn h $ show (floor $ x/k :: Int)
          load `on` buttonActivated $ do
                        mh <- atomically $ readTVar thandle
                        case mh of 
                                Nothing -> return ()
                                Just h -> do
                                        l <- hGetLine h
                                        let (wx :: Int) = read l      
                                        progressBarSetFraction level (fromIntegral wx * k)
                                        labelSetText label $ show wx
                                        labelSetText param $ show paramv
          dupchan <- atomically $ dupTChan midiinchan
          forkIO . forever $ do
                         
                        (tp,wx) <- atomically $ readTChan dupchan
                        case tp == paramv of 
                                False -> return ()
                                True ->  postGUISync $ do 
                                        wx' <- read `fmap` (labelGetText label)
                                        case  wx' /= wx of
                                                False -> return ()
                                                True -> do 
                                                        progressBarSetFraction level (fromIntegral wx * k)
                                                        labelSetText label $ show wx


          res `on` buttonActivated $ do
                        x <- progressBarGetFraction level 
                        atomically $ writeTChan midioutchan (paramv,floor $ x/k)
          on eb motionNotifyEvent $ do 
                (r,_) <- eventCoordinates
                (r',0) <- liftIO $ fmap read $ labelGetText memory
                liftIO $ do 
                        x <- progressBarGetFraction level 
                        let y = (if r > r' then limitedAdd 1 else limitedSubtract 0) k x
                        progressBarSetFraction level y
                        atomically $ writeTChan midioutchan (paramv,floor $ y/k)
                        labelSetText label $ show (floor $ y/k)
                        labelSetText memory $ show (r,0)
                return True
          on level scrollEvent $  tryEvent $ do 
                ScrollUp <- eventScrollDirection
                liftIO $ do 
                        x <- progressBarGetFraction level 
                        let y = limitedAdd 1 k x
                        atomically $ writeTChan midioutchan (paramv,floor $ y/k)
                        progressBarSetFraction level y
                        labelSetText label $ show (floor $ y/k)
          on level scrollEvent $  tryEvent $ do 
                ScrollDown <- eventScrollDirection
                liftIO $ do 
                        x <- progressBarGetFraction level 
                        let y = limitedSubtract 0 k x
                        atomically $ writeTChan midioutchan (paramv,floor $ y/k)
                        progressBarSetFraction level y
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
