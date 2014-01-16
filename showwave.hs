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


collapse 0 = 0
collapse 1 = 0
collapse 2 = 2
collapse 3 = 3
collapse 4 = 3
collapse 5 = 5
collapse 6 = 7
collapse 7 = 7
collapse 8 = 8
collapse 9 = 8
collapse 10 = 10
collapse 11 = 12

modcollapse x = let
	(o,n) = x `divMod` 12
	in o * 12 + collapse n

select 0 = _1
select 1 = _2
select 2 = _3

main :: IO ()
main = do
  tw <- newTVarIO (M.fromList . zip [0..3] . repeat . M.fromList . zip [0..7] $ repeat (0::GLfloat,0,0))
  tl <- newTVarIO 0
  tc <- newTChanIO
  forkIO $ midiIn "sins_graph" 0 tc
  forkIO . forever $ do 
		(n,s) <- atomically (readTChan tc) 
		when (n<96) $ 
			atomically . modifyTVar tw . flip M.adjust (n `div` 24) . flip M.adjust (n `mod` 8) $  select (n `mod` 24 `div` 8) .~ fromIntegral s
  
		case n of 
			125 -> atomically $ writeTVar tl $ fromIntegral s/128
			_ -> return ()
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
    clearColor $= (Color4 1.0 0.8 1.0 0.0)
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
      l <- atomically $ readTVar tl
      draw w l
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
draw ws l = do
  matrixMode $= Modelview 0
  loadIdentity
  renderPrimitive LineStrip $ do
		color4f 0 0 0 1
                forM_ [0,0.001 .. 1] $ \x -> vertex2f x (  sum . map (^4) $ [a/16*sin (s/128*2*pi + x*2*pi*w)/10 | (w,s,a) <- M.elems (ws M.! 0)])
  forM_ [0..63]	$ \x ->  renderPrimitive LineStrip $ do
		color4f 0.5 0.6 0.7 1
		vertex2f (x/64) 0
		vertex2f (x/64) 1
  renderPrimitive LineStrip $ do
		color4f 0.3 0.6 0.3 1
		vertex2f 0 l
		vertex2f 1 l
