{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import Data.IORef

import Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))
import Graphics.UI.Gtk.OpenGL as GtkGL
import Control.Monad
import System.Random
import Control.Concurrent.STM
import Control.Monad.Trans

import Graphics.Rendering.OpenGL as GL

import Track
import Simple

main :: IO ()
main = do
  initGUI

  -- Initialise the Gtk+ OpenGL extension
  -- (including reading various command line parameters)
  initGL

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

  -- Create an OpenGL drawing area widget
  canvas <- glDrawingAreaNew glconfig

  widgetSetSizeRequest canvas 320 800

  -- Initialise some GL setting just before the canvas first gets shown
  -- (We can't initialise these things earlier since the GL resources that
  -- we are using wouldn't heve been setup yet)
  onRealize canvas $ withGLDrawingArea canvas $ \_ -> do
    clearColor $= (Color4 1.0 1.0 1.0 0.0)
    matrixMode $= Projection
    loadIdentity
    ortho 0.0 1.0 0.0 1.0 (-1.0) 1.0
    depthFunc $= Just Less
    drawBuffer $= BackBuffers

  ref <- newTVarIO (0, [0..11],db)

  -- Set the repaint handler
  onExpose canvas $ \_ -> do
    withGLDrawingArea canvas $ \glwindow -> do
      clear [DepthBuffer, ColorBuffer]
      drawCube ref 
      glDrawableSwapBuffers glwindow
    return True
  canvas `on` buttonPressEvent $ do
        (x,y) <- eventCoordinates
        liftIO $ do
                (_,ny,_) <- atomically $ readTVar ref
                w <- widgetGetDrawWindow canvas
                (mx,my) <- drawableGetSize w
                print (x/fromIntegral mx,floor $ y / fromIntegral my  * fromIntegral (length ny))
        return True
  -- Setup the animation
  timeoutAddFull (do
    atomically $ modifyTVar ref (\(t, ns, db) -> (t + 0.005, ns, db))
    widgetQueueDraw canvas
    return True)
    priorityHigh 10

  --------------------------------
  -- Setup the rest of the GUI:
  --
  window <- windowNew
  onDestroy window mainQuit
  set window [ containerBorderWidth := 8,
                   windowTitle := "tracks widget" ]

  sc <- scrolledWindowNew Nothing Nothing
  
  frame <- frameNew
  scrolledWindowAddWithViewport sc frame
  set frame [containerChild := canvas]
  set window [containerChild := sc] 
  widgetShowAll window
  mainGUI

zz1 = 1/256
drawCube :: TVar (GLfloat,[Int], DBTrack) -> IO ()
drawCube  state = do
  (t,ms,db) <- atomically $ readTVar state
  matrixMode $= Modelview 0
  loadIdentity
  let roll x = x -- floatMod (x - t') 1
  -- let   xyhs = map (\(x,y,h) -> (roll x,y,h)) xyhs'
  --       t = 0.5
  let sr x c1 c2 c3 c4 = let 
        r c = c -- 4 * (x - 0.5) ^ 2 + c
        in Color4 (r c1) (r c2) (r c3) c4
  let ns = realToFrac . fromIntegral $ length ms
  forM_ ms $ \n -> do 
        let  sc = scoreOfTrack db n
             lh h = 1/ns * realToFrac (min 1 h)
             y = 1/ns * fromIntegral n
        forM_ sc $ \(realToFrac -> x,lh -> h) -> renderPrimitive Quads $ do
                color (sr x 0.6 0.7 0.4 1 :: Color4 GLfloat)
                vertex (Vertex2 x  y  :: Vertex2 GLfloat)
                vertex (Vertex2 (x + zz1) y :: Vertex2 GLfloat)
                color (sr x 0.2 0.3 0 1 :: Color4 GLfloat)
                vertex (Vertex2 (x + zz1) (y + h) :: Vertex2 GLfloat)
                vertex (Vertex2 x (y + h):: Vertex2 GLfloat)
  forM_ [0,1/32..1] $ \x -> renderPrimitive Quads $ do
                color (sr (roll x) 0.8 0.8 0.8  0.1 :: Color4 GLfloat)
                vertex (Vertex2 (roll x) 0   :: Vertex2 GLfloat)
                vertex (Vertex2 (roll x) 1 :: Vertex2 GLfloat)
                vertex (Vertex2 (roll x + zz1)  1   :: Vertex2 GLfloat)
                vertex (Vertex2 (roll x + zz1) 0 :: Vertex2 GLfloat)


  renderPrimitive Quads $ do
                color (Color4 0.8 0.8 0.8 0.1 :: Color4 GLfloat)
                vertex (Vertex2 (floatMod t 1 - 3*zz1) 0 :: Vertex2 GLfloat)
                vertex (Vertex2 ((floatMod t 1) - 3*zz1) 1 :: Vertex2 GLfloat)
                vertex (Vertex2 (floatMod t 1 - 1*zz1) 1 :: Vertex2 GLfloat)
                vertex (Vertex2  (floatMod t 1 - 1*zz1) 0 :: Vertex2 GLfloat)
  renderPrimitive Quads $ do
                color (Color4 0.8 0.8 0.8 0.1 :: Color4 GLfloat)
                vertex (Vertex2 (floatMod t 1 + 3*zz1) 0 :: Vertex2 GLfloat)
                vertex (Vertex2 ((floatMod t 1) + 3*zz1) 1 :: Vertex2 GLfloat)
                vertex (Vertex2 (floatMod t 1 + 1*zz1) 1 :: Vertex2 GLfloat)
                vertex (Vertex2  (floatMod t 1 + 1*zz1) 0 :: Vertex2 GLfloat)
  forM_ [0 .. ns - 1] $ \x -> renderPrimitive Quads $ do
                color (Color4 0.6 1 0.6 0.1 :: Color4 GLfloat)
                vertex (Vertex2 0 (x/ns)   :: Vertex2 GLfloat)
                color (Color4 0.7 1 0.7 0.1 :: Color4 GLfloat)
                vertex (Vertex2 1 (x/ns) :: Vertex2 GLfloat)
                color (Color4 1 1 1 0.1 :: Color4 GLfloat)
                vertex (Vertex2 1 ((x+1)/ns )  :: Vertex2 GLfloat)
                color (Color4 1 1 1 0.1 :: Color4 GLfloat)
                vertex (Vertex2 0 ((x+1)/ns) :: Vertex2 GLfloat)



floatMod x y = let
        z = x/y
        fz = fromIntegral $ floor z
        in z - fz
to, from :: GLfloat
to = 0.4
from   =  -0.4

animationWaitTime :: Int
animationWaitTime = 3

dx, dy, dz :: GLfloat
dx = 0.001
dy = 0.003
dz = 0.07

red, green, yellow, blue, purple, cyan :: Color3 GLfloat
red    = Color3 1 0 0
green  = Color3 0 1 0
yellow = Color3 1 1 0
blue   = Color3 0 0 1
purple = Color3 1 0 1
cyan   = Color3 0 1 1
