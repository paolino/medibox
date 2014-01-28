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



import Wave

main :: IO ()
main = do
  tw <- newTVarIO initWave
  ha <- newTVarIO (0,0)
  forkIO $ mantain "showwave" tw ha
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

  widgetSetSizeRequest canvas 1000 700
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
      draw w 
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
draw ws  = do
  matrixMode $= Modelview 0
  loadIdentity
  forM_ [0..7] $ \k -> do
        let     inst = ws M.! k
                y0 = fromIntegral k / 8 
        forM_ [0..3] $ \nw -> do
                let     wave = inst M.! nw
                renderPrimitive LineStrip $ do
                               case nw of 
                                        0 -> color4f 0 0 0 1
                                        1 -> color4f 1 0 0 1
                                        2 -> color4f 0 1 0 1
                                        3 -> color4f 0 0 1 1
                               forM_ [0,0.001 .. 1] $ \x -> vertex2f x (y0 + realToFrac (evalWave wave (realToFrac x * 2* pi))/8)
				
  forM_ [0..32]	$ \x ->  renderPrimitive LineStrip $ do
		color4f 0.5 0.6 0.7 1
		vertex2f (x/32) 0
		vertex2f (x/32) 1
