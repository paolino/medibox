{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import Data.IORef

import Graphics.UI.Gtk as Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Cairo
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk (AttrOp((:=)))
import Graphics.UI.Gtk.OpenGL as GtkGL
import Control.Monad
import System.Random
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad.Trans
import Sound.OSC
import Graphics.Rendering.OpenGL as GL

import Control.Lens ((^.), at, traverse, (%~), _3,_4,(.~),_1,_2)
import MidiComm
import qualified Data.Map as M

delta = 0.120 * 256
pos t = let 
        n = floor (t / delta)
        z = fromIntegral n * delta
        in floor $ (t - z)/delta * 256


        
        
core :: Int -> TChan (Int,Int,Int) -> TVar (M.Map Int [(Int,Int,Int)]) -> IO ()
core p' mi re  = do
        n <- atomically $ readTChan mi  
        p <- pos `fmap` time
        atomically $ 
                if p /= p' then do
                          modifyTVar re . flip M.adjust p $ (\_ -> [n]) 
                else do 
                        modifyTVar re . flip M.adjust p $ (n:)
        print (n,p)
        core p mi  re 
        

main :: IO ()
main = do
  tw <- newTVarIO (M.fromList $ zip  [0..256-1 :: Int] $ repeat [])
  tc <- newTChanIO
  forkIO $ core 0 tc tw
  forkIO $ midiInNoteOn "notes_graph" tc
  initGUI
  

  -- Initialise the Gtk+ OpenGL extension
  -- (including reading various command line parameters)
  initGL
  -- initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]  
  depthFunc $= Just Less -- specifies comparison function for DepthBuffer

  clearColor $= Color4 0 0 0 1
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  lineSmooth $= Enabled
  pointSmooth $= Enabled
  polygonSmooth $= Enabled
  shadeModel $= Smooth
  -- We need a OpenGL frame buffer configuration to be able to create other
  -- OpenGL objects.
  glconfig <- glConfigNew [GLModeRGBA,
                                 GLModeDepth,
                                 GLModeDouble]
  
  canvas <- glDrawingAreaNew glconfig

  widgetSetSizeRequest canvas 1000 500
  -- Initialise some GL setting just before the canvas first gets shown
  -- (We can't initialise these things earlier since the GL resources that
  -- we are using wouldn't heve been setup yet)
  onRealize canvas $ withGLDrawingArea canvas $ \_ -> do
    clearColor $= (Color4 1.0 1 1.0 0.0)
    matrixMode $= Projection
    loadIdentity
    ortho 0.0 1.0 0.0 1.0 (-1.0) 1.0
    depthFunc $= Just Less
    drawBuffer $= BackBuffers


  -- Set the repaint handler
  onExpose canvas $ \_ -> do
    withGLDrawingArea canvas $ \glwindow -> do
      clear [DepthBuffer, ColorBuffer]
      w <- atomically $ readTVar tw
      draw (M.assocs w)
      glDrawableSwapBuffers glwindow
    return True
  
  flip timeoutAdd 100 (do
    widgetQueueDraw canvas
    return True)
    -- priorityLow
  --------------------------------
  -- Setup the rest of the GUI:
  --
  window <- windowNew
  onDestroy window mainQuit
  set window [ containerBorderWidth := 8,
                   windowTitle := "tracks widget", containerChild := canvas ]
  widgetShowAll window
  dat <- widgetGetDrawWindow $ window
  cursorNew Tcross >>= drawWindowSetCursor dat . Just 
  mainGUI

color4f r g b a = color $ Color4 r g (b :: GLfloat) a
vertex2f x y = vertex $ Vertex2 x (y :: GLfloat)
draw ws = do
  matrixMode $= Modelview 0
  loadIdentity
  renderPrimitive LineStrip $ do
                forM_ ws $ \(t,xs) -> 
                        forM_ xs $ \(c,p,f) -> do
                                when (c == 5) $ do 
                                        color4f 0.5 0.2 0.7 1
                                        renderPrimitive LineStrip $ forM_ [0,0.1..2 * pi] $ \a -> do
                                                vertex2f (cos a * fromIntegral f/512 + fromIntegral t/256) ((sin a * fromIntegral f/512 + fromIntegral p/128)/1)

