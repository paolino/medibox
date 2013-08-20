{-# LANGUAGE ScopedTypeVariables #-}
module GUI where

import Graphics.UI.Gtk
import Control.Monad.Trans
import Control.Monad

import qualified Data.IntMap as IM
import Control.Concurrent
import System.IO


import Sound.OSC

import Control.Concurrent.STM
import MidiComm
import Graphics.UI.Gtk.Gdk.GC
import Sequenza
import Haskell
import Interface
import PersistentChoiceLens     
import Control.Lens  hiding (set)
import Projections
import Instr
import Realize

k = 1/128


patternDraw dr mn tpresence tsel = do 
        ds <- atomically $ do
                mpg <- readTVar tsel
                let prog = mpg IM.! mn
                Interface seqs projs ssamples <- readTVar tpresence
                let     Realize (i,j) ssample = ssamples IM.! fromIntegral prog
                case querySequenza seqs j  of
                        Nothing -> return Nothing
                        Just (_,sc,nseqs) -> do             
                                writeTVar tpresence $ Interface nseqs projs ssamples
                                return $ Just $ project sc (projs IM.! i)
        case ds of 
                Just ds -> do 
                          wdr <- widgetGetDrawWindow dr
                          -- drawableGetSize :: DrawableClass d => d -> IO (Int, Int)
                          (mx,my) <- drawableGetSize wdr
                          let   ds' = (map (\(x,y) -> (floor $ x * fromIntegral mx, y)))  ds
                                dmy = maximum $ map snd ds'
                                ds''= map (\(x,y) -> (x,floor $ y * fromIntegral my )) ds'
                          gc <- gcNewWithValues wdr newGCValues -- {foreground = Color 255 255 255, background = Color 0 0 0}
                          -- drawLine :: DrawableClass d => d -> GC -> Point -> Point -> IO ()
                          forM_ ds'' $ \(x,y) -> drawRectangle  wdr gc True x (my - y) 3 my
                Nothing -> return ()


gui :: (Int ->  String) -> TChan (Int,Int) -> TChan (Int,Int) ->  TVar Interface -> TVar (IM.IntMap Int) -> IO ()
gui snames midiinchan midioutchan tseq tselection = do
  thandle <- newTVarIO Nothing


  initGUI
  window <- windowNew

  mbox <- hBoxNew True 1 
  set window [windowDefaultWidth := 200, windowDefaultHeight := 200,
                          containerBorderWidth := 10, containerChild := mbox]
  forM_ [0..2] $ \mn -> do
          let sh x = mn * 41 + x

          vbox    <- vBoxNew False 1
          frame <- frameNew 
          set frame [containerChild:= vbox]
          boxPackStart mbox frame PackGrow 0
          synth <- labelNew  $ Just "synthname"
          frame <- frameNew 
          set frame [containerChild:= synth]
          boxPackStart vbox frame PackNatural 0

          box    <- hBoxNew False 1
          frame <- frameNew 
          set frame [containerChild:= box]
          boxPackStart vbox frame PackGrow 0
          controllo midiinchan midioutchan Nothing box (const $ return ()) $ sh 40
          forM_ [sh 29..sh 30] $ \i -> controllo midiinchan midioutchan Nothing box (const $ return ()) $ i
          controllo midiinchan midioutchan Nothing box (\v -> labelSetText synth $ snames v) $ sh 31
          forM_ [sh 32..sh 39] $ \i -> controllo midiinchan midioutchan Nothing box (const $ return ()) $ i

          bbox <- hBoxNew False 1
          frame <- frameNew 
          set frame [containerChild:= bbox]
          boxPackEnd vbox frame PackGrow 0
          dr <- drawingAreaNew
          dr `on`  exposeEvent $  do liftIO $ patternDraw dr mn tseq tselection >> return True
          forkIO . forever $ do sleepThread 0.1 >> postGUIAsync (widgetQueueDraw dr)
          boxPackEnd bbox dr PackGrow 0

          forM_ [0..2] $ \paramt -> do
             pbox <- hBoxNew False 1
             frame <- frameNew 
             set frame [containerChild:= pbox]

             boxPackStart vbox frame PackGrow 0
             forM_ [0..7] $ \params -> controllo midiinchan midioutchan (Just dr) pbox (const $ return ()) $  sh $ paramt * 8 + params

          pbox <- hBoxNew False 1
          frame <- frameNew 
          set frame [containerChild:= pbox]
          boxPackStart vbox frame PackGrow 0
          forM_ [0..4] $ \paramv -> controllo midiinchan midioutchan (Just dr) pbox (const $ return ()) $  sh $ paramv + 24


  onDestroy window mainQuit
  widgetShowAll window
  mainGUI

limitedAdd xm d x
        | x + d > xm = xm
        | otherwise = x + d
limitedSubtract xm d x
        | x - d < xm = xm
        | otherwise = x - d
controllo
  :: (BoxClass self) =>
     TChan (Int, Int)
     -> TChan (Int, Int)
     -> Maybe DrawingArea
     -> self
     -> (Int -> IO ())
     -> Int
     -> IO (ConnectId ProgressBar)
controllo midiinchan midioutchan dr pbox supp paramv = do
          hbox    <- vBoxNew False 1
          widgetSetSizeRequest hbox 33 60
          boxPackStart pbox hbox PackNatural 0

          param <- labelNew (Just $ show paramv)
          frame <- frameNew
          set frame [containerChild:= param]
          boxPackStart hbox frame PackNatural 0

          label <- labelNew (Just "0")
          frame <- frameNew
          set frame [containerChild:= label]
          boxPackStart hbox frame PackNatural 0

          eb <- eventBoxNew
          level <- progressBarNew 
          progressBarSetOrientation level ProgressBottomToTop
          set eb [containerChild:= level]
          
          widgetAddEvents eb [Button1MotionMask]
          boxPackStart hbox eb PackGrow 0
          progressBarSetFraction level 0
          memory <- labelNew (Just $ show (0,0))
          dupchan <- atomically $ dupTChan midiinchan
          forkIO . forever $ do
                         
                        (tp,wx) <- atomically $ readTChan dupchan
                        case tp == paramv of 
                                False -> return ()
                                True ->  postGUISync $ do 
                                        supp wx
                                        wx' <- read `fmap` (labelGetText label)
                                        case  wx' /= wx of
                                                False -> return ()
                                                True -> do 
                                                        progressBarSetFraction level (fromIntegral wx * k)
                                                        labelSetText label $ show wx


          on eb motionNotifyEvent $ do 
                (_,r) <- eventCoordinates
                (0,r') <- liftIO $ fmap read $ labelGetText memory
                liftIO $ do 
                        x <- progressBarGetFraction level 
                        let y = (if r' > r then limitedAdd 1 else limitedSubtract 0) k x
                        progressBarSetFraction level y
                        atomically $ writeTChan midioutchan (paramv,floor $ y/k)
                        labelSetText label $ show (floor $ y/k)
                        labelSetText memory $ show (0,r)
                        supp $ floor $ y/k
                return True
          on level scrollEvent $  tryEvent $ do 
                ScrollUp <- eventScrollDirection
                liftIO $ do 
                        x <- progressBarGetFraction level 
                        let y = limitedAdd 1 k x
                        atomically $ writeTChan midioutchan (paramv,floor $ y/k)
                        progressBarSetFraction level y
                        labelSetText label $ show (floor $ y/k)
                        supp $ floor $ y/k
          on level scrollEvent $  tryEvent $ do 
                ScrollDown <- eventScrollDirection
                liftIO $ do 
                        x <- progressBarGetFraction level 
                        let y = limitedSubtract 0 k x
                        atomically $ writeTChan midioutchan (paramv,floor $ y/k)
                        progressBarSetFraction level y
                        labelSetText label $ show (floor $ y/k)
                        supp $ floor $ y/k

{-
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

          res `on` buttonActivated $ do
                        x <- progressBarGetFraction level 
                        atomically $ writeTChan midioutchan (paramv,floor $ x/k)
-}
{-
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
                                        widgetQueueDraw dr
-}  
{-
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
-}
