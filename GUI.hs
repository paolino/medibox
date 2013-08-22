{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}
module GUI where

import Graphics.UI.Gtk
import Control.Monad.Trans
import Control.Monad

import qualified Data.IntMap as IM
import Control.Concurrent
import System.IO
import Control.Arrow


import Sound.OSC

import Control.Concurrent.STM
import MidiComm
import Graphics.UI.Gtk.Gdk.GC
import Sequenza
import Graphics.Rendering.Cairo
import Haskell
import Interface
import PersistentChoiceLens     
import Control.Lens  hiding (set, moveTo)
import Projections
import Instr
import Realize

k = 1/128

myDraw _ = do
    setSourceRGB 1 1 0
    setLineWidth 4

    moveTo 120 60
    lineTo 60 110
    lineTo 180 110
    closePath
    
    stroke   
visio ((+ 2) -> t) yt xs = do 
        
    setSourceRGB 0 0 0
    setLineWidth 4
    forM_ xs $ \((+ 2) -> x,(+ 2) -> y,h) -> do
        moveTo x y
        lineTo x (y + h)
    setSourceRGB 1 0 0
    setLineWidth 4
    moveTo t 0
    lineTo t yt
    stroke
patternDraw dr mn tpresence tsel ttimeperc  = do 
        wdr <- widgetGetDrawWindow dr 
        let ds mn =  atomically $ do 
                
                mpg <- readTVar tsel
                t <- readTVar ttimeperc
                let prog = mpg IM.! (mn `mod` 3)
                Interface seqs projs ssamples <- readTVar tpresence
                let     Realize (i,j) ssample = ssamples IM.! fromIntegral prog
                case querySequenza seqs j  of
                        Nothing -> return  ([],t) 
                        Just (_,sc,nseqs) -> do             
                                writeTVar tpresence $ Interface nseqs projs ssamples
                                return $ (project sc (projs IM.! i),t)
        (mx,my) <- drawableGetSize wdr
        renderWithDrawable wdr . forM_ [0..mn - 1] $ \z -> do 
          (ds',t) <- liftIO $ first (map (\(x,y) -> (x * fromIntegral mx, y))) `fmap`  ds z
          let   dmy = maximum $ map snd ds'
                ds''= map (\(x,y) -> (x,fromIntegral z * fromIntegral my / fromIntegral mn, y * fromIntegral my / fromIntegral mn   )) ds'
          visio (t * fromIntegral mx) (fromIntegral my) ds''
        return True

        
        


gui :: (Int ->  String) -> TChan (Int,Int) -> TChan (Int,Int) ->  TVar Interface -> TVar (IM.IntMap Int) -> TVar Double -> IO ()
gui snames midiinchan midioutchan tseq tselection ttimeperc = do
  thandle <- newTVarIO Nothing


  initGUI
  window <- windowNew

  mbox <- vBoxNew True 1 
  set window [windowDefaultWidth := 200, windowDefaultHeight := 200,
                          containerBorderWidth := 10, containerChild := mbox]
        {-
  drs <- forM [0..10] $ \mn -> do
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
          return (mn)
-}

  dr <- drawingAreaNew
  dr `on`  exposeEvent $  do liftIO $ patternDraw dr 10 tseq tselection ttimeperc >> return True 
  boxPackEnd mbox dr PackGrow 0
  timeoutAddFull (widgetQueueDraw dr >> return True) priorityHigh 20

        

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
