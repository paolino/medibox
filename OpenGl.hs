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
import Track
import Simple
import qualified Score
import Haskell
import OpenGlDigits
data Poin = 
        Tr Int Int
        | Sq (String, Double, Double)
        | Pj (String, Double, Double)
main :: IO ()
main = do
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
  
  ref <- newTVarIO (0, [0..11],db,Tr 0 0)
  -- Create an OpenGL drawing area widget


  let canva (dx,dy,draw) = do
          canvas <- glDrawingAreaNew glconfig

          widgetSetSizeRequest canvas dx dy

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


          -- Set the repaint handler
          onExpose canvas $ \_ -> do
            withGLDrawingArea canvas $ \glwindow -> do
              clear [DepthBuffer, ColorBuffer]
              draw glwindow ref 
              glDrawableSwapBuffers glwindow
            return True
          return canvas
  tracks <- canva (320,900,drawCube )
  projs <- canva (120,900,drawProj)
  seqs <- canva (480,900,drawSeqs)
  -- Setup the animation
  t0 <- time
  forkIO. forM_ [0..] $ \n -> do
        sleepThreadUntil $ t0 + n * 1/100
        atomically (modifyTVar ref (\(t, ns, db,s) -> (t + 1/400, ns, db,s)))
  idleAdd (do
    widgetQueueDraw tracks
    return True)
    priorityLow
  idleAdd (do
    widgetQueueDraw projs
    return True)
    priorityLow
  idleAdd (do
    widgetQueueDraw seqs
    return True)
    priorityLow
  --------------------------------
  -- Setup the rest of the GUI:
  --
  window <- windowNew
  onDestroy window mainQuit
  set window [ containerBorderWidth := 8,
                   windowTitle := "tracks widget" ]
  hb <- hBoxNew False 1
  frame <- frameNew
  set frame [containerChild := tracks]
  boxPackStart hb frame PackNatural 0 
  frame <- frameNew
  set frame [containerChild := projs]
  boxPackStart hb frame PackNatural 0 
  frame <- frameNew
  set frame [containerChild := seqs]
  boxPackStart hb frame PackNatural 0 
  widgetAddEvents projs [PointerMotionMask]
  widgetAddEvents seqs [PointerMotionMask]
  widgetAddEvents tracks [PointerMotionMask]
  let prlens = ([Score.prampl, Score.proffset, Score.prwidth, Score.prshift, Score.prquant, Score.prcutin] !!)
  let prdesc = (["AMPLIFICATION","OFFSET","WIDTH","SHIFT","QUANTIFICATION","CUTIN"] !!)
  on projs motionNotifyEvent $ do
        (x,y) <- eventCoordinates
        let (p,n,_) =  (floor $ x / 20,floor $ 12 - (y *12 / 900),floor $ 900/12 - (floatMod y $ 900/12))
        liftIO $ do 
                atomically $ modifyTVar ref $ _4 .~ Pj (prdesc p,1/121,(fromIntegral n + 0.8)/ 12) 
                return True
        
  on projs scrollEvent $  tryEvent $ do 
        ScrollUp <- eventScrollDirection
        (x,y) <- eventCoordinates
        let (p,n,_) =  (floor $ x / 20,floor $ 12 - (y *12 / 900),floor $ 900/12 - (floatMod y $ 900/12))
        liftIO $ do 
                (t,ms,db,s) <- atomically $ readTVar ref
                let  Just (Track i j) = db ^. dbtrack n
                atomically $ modifyTVar ref $ _3 . dbproj j . traverse . (prlens p) %~ (min 127 .(+1))
        
  on projs scrollEvent $  tryEvent $ do 
        ScrollDown <- eventScrollDirection
        (x,y) <- eventCoordinates
        let (p,n,_) =  (floor $ x / 20,floor $ 12 - (y *12 / 900),floor $ 900/12 - (floatMod y $ 900/12))
        
        liftIO $ do 
                (t,ms,db,s) <- atomically $ readTVar ref
                let  Just (Track i j) = db ^. dbtrack n
                atomically $ modifyTVar ref $ _3 . dbproj j . traverse . (prlens p) %~ (max 0.(subtract 1))
  
  let sqlens = ([Score.pnumber, Score.pwidth, Score.pshift] !!)
  let sqdesc = (["NUMBER","WIDTH","OFFSET"] !!)
  on seqs motionNotifyEvent $ do
        (x,y) <- eventCoordinates
        let (q,p,n,_) =  (floor $ floatMod (x/20) 3, floor $ x / 20 / 3,floor $ 12 - (y *12 / 900),floor $ 900/12 - (floatMod y $ 900/12))
        liftIO $ do 
                atomically $ modifyTVar ref $ _4 .~ Sq (sqdesc q,fromIntegral (p*3)/24 + 1/480 ,(fromIntegral n + 0.8)/ 12) 
                return True

  on seqs scrollEvent $  tryEvent $ do 
        ScrollUp <- eventScrollDirection
        (x,y) <- eventCoordinates
        let (q,p,n,_) =  (floor $ floatMod (x/20) 3, floor $ x / 20 / 3,floor $ 12 - (y *12 / 900),floor $ 900/12 - (floatMod y $ 900/12))
        liftIO $ do 
                (t,ms,db,s) <- atomically $ readTVar ref
                let  Just (Track i j) = db ^. dbtrack n
                atomically $ modifyTVar ref $ _3 . dbseq i . traverse . at p . traverse . (sqlens q) %~ (min 127 .(+1))
  on seqs scrollEvent $  tryEvent $ do 
        ScrollDown <- eventScrollDirection
        (x,y) <- eventCoordinates
        let (q,p,n,_) =  (floor $ floatMod (x/20) 3, floor $ x / 20 / 3,floor $ 12 - (y *12 / 900),floor $ 900/12 - (floatMod y $ 900/12))
        liftIO $ do 
                (t,ms,db,s) <- atomically $ readTVar ref
                let  Just (Track i j) = db ^. dbtrack n
                atomically $ modifyTVar ref $ _3 . dbseq i . traverse . at p . traverse . (sqlens q) %~ (max 0 .(subtract 1))

  on tracks  motionNotifyEvent $ do
        (x,y) <- eventCoordinates
        let (q,p,n,_) =  (0,0,floor $ 12 - (y *12 / 900),0)
        liftIO $ do 
                (t,ms,db,s) <- atomically $ readTVar ref
                let  Just (Track i j) = db ^. dbtrack n
                atomically $ modifyTVar ref $ _4 .~ Tr i j
                return True
      
  sc <- scrolledWindowNew Nothing Nothing
  
  frame <- frameNew
  set frame [containerChild := hb]
  scrolledWindowAddWithViewport sc frame
  set window [containerChild := sc] 
  widgetShowAll window
  mainGUI

zz1 = 1/256
-- drawCube :: DrawingArea -> TVar (GLfloat,[Int], DBTrack) -> IO ()
drawCube  _ state = do
  (t,ms,db,s) <- atomically $ readTVar state
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
        let  sc = maybe [] id $ scoreOfTrack db n
             lh h = 1/ns * realToFrac (0.9 * (min 1 h))
             y = 1/ns * fromIntegral n
             Just (Track i j) = db ^. dbtrack n
             Just (Score.Projection a o w s q c) = db ^. dbproj j
        forM_ sc $ \(realToFrac -> x,lh -> h) -> renderPrimitive Quads $ do
                color (sr x 0.6 0.7 0.4 1 :: Color4 GLfloat)
                vertex (Vertex2 x  y  :: Vertex2 GLfloat)
                vertex (Vertex2 (x + zz1*2) y :: Vertex2 GLfloat)
                color (sr x 0.2 0.3 0 1 :: Color4 GLfloat)
                vertex (Vertex2 (x + zz1*2) (y + h) :: Vertex2 GLfloat)
                vertex (Vertex2 x (y + h):: Vertex2 GLfloat)
        
  let t' = floatMod t 1
  renderPrimitive Quads $ do
                color (Color4 0.7 0.7 0.7 0.1 :: Color4 GLfloat)
                vertex (Vertex2 (t' - 3*zz1) 0 :: Vertex2 GLfloat)
                vertex (Vertex2 (t' - 3*zz1) 1 :: Vertex2 GLfloat)
                vertex (Vertex2 (t' - 1*zz1) 1 :: Vertex2 GLfloat)
                vertex (Vertex2  (t' - 1*zz1) 0 :: Vertex2 GLfloat)
  renderPrimitive Quads $ do
                color (Color4 0.7 0.7 0.7 0.1 :: Color4 GLfloat)
                vertex (Vertex2 (t' + 3*zz1) 0 :: Vertex2 GLfloat)
                vertex (Vertex2 (t' + 3*zz1) 1 :: Vertex2 GLfloat)
                vertex (Vertex2 (t' + 1*zz1) 1 :: Vertex2 GLfloat)
                vertex (Vertex2  (t' + 1*zz1) 0 :: Vertex2 GLfloat)
  forM_ (ap zip tail [0,1/ns .. 1]) $ \(y1,y2) -> renderPrimitive Quads $ do
                color (Color4 0.6 1 0.6 0.1 :: Color4 GLfloat)
                vertex (Vertex2 0 y1   :: Vertex2 GLfloat)
                color (Color4 0.7 1 0.7 0.1 :: Color4 GLfloat)
                vertex (Vertex2 1 y1 :: Vertex2 GLfloat)
                color (Color4 1 1 1 0.1 :: Color4 GLfloat)
                vertex (Vertex2 1 y2  :: Vertex2 GLfloat)
                color (Color4 1 1 1 0.1 :: Color4 GLfloat)
                vertex (Vertex2 0 y2 :: Vertex2 GLfloat)



drawProj glwindow state = do
  (t,ms,db,s) <- atomically $ readTVar state
  matrixMode $= Modelview 0
  loadIdentity
  let ns = realToFrac . fromIntegral $ length ms

  color (Color4 0.4 0.4 0.4 0.2 :: Color4 GLfloat)
  mj <- case s of
        Pj s ->  renderWordPOSWH 0.5 0.5 (s ^. _2) (s ^. _3) (1/30) (1/70) (s ^. _1) >> return Nothing
        Tr i j -> return $ Just j
        _ -> return Nothing 
  let mark j' = case mj of 
        Nothing -> id
        Just j -> if j == j' then subtract 0.1 else id 
  forM_ [0 .. ns -1] $ \nt -> do
                case db ^. dbproj (floor nt) of 
                        Nothing -> return ()
                        Just (Score.Projection a b c d e f) -> do 
                                let     y = nt/ns
                                        fy = mark $ floor nt
                                renderPrimitive LineLoop $ do 
                                                color (Color4 0.6 0.6 0.6 0.3 :: Color4 GLfloat)
                                                vertex (Vertex2 (0) (y) :: Vertex2 GLfloat)
                                                vertex (Vertex2  1 (y):: Vertex2 GLfloat)
                                                color (Color4 0.4 0.6 0.6 0.3 :: Color4 GLfloat)
                                                vertex (Vertex2 1 (y+1/ns) :: Vertex2 GLfloat)
                                                vertex (Vertex2 (0) (y+1/ns) :: Vertex2 GLfloat)
                                forM_ (zip (ap zip tail [0,1/6..]) [a,b,c,d,e,f]) $ \((x1,x2), a) -> do
                                        color (Color4 0.4 0.4 0.4 1 :: Color4 GLfloat)
                                        renderNumberPosWH 0.5 0.5 (realToFrac x2 - 1/15) (realToFrac y + 1/200) (1/25) (1/80) a
                                        renderPrimitive LineLoop $ do 
                                                color (Color4 0.8 0.8 0.8 0.3 :: Color4 GLfloat)
                                                vertex (Vertex2 x1 (y) :: Vertex2 GLfloat)
                                                vertex (Vertex2  x2 (y):: Vertex2 GLfloat)
                                                color (Color4 0.9 0.9 0.9 0.3 :: Color4 GLfloat)
                                                vertex (Vertex2 x2 (y+1/ns) :: Vertex2 GLfloat)
                                                vertex (Vertex2 x1 (y+1/ns) :: Vertex2 GLfloat)
                                        renderPrimitive Quads $ do 
                                                color (Color4 (fy 0.9) (fy 0.9) (fy 1) 0.3 :: Color4 GLfloat)
                                                vertex (Vertex2 x1 (y) :: Vertex2 GLfloat)
                                                vertex (Vertex2  x2 (y):: Vertex2 GLfloat)
                                                color (Color4 (fy 0.6) (fy 0.6) (fy 1) 0.3 :: Color4 GLfloat)
                                                vertex (Vertex2 x2 (y+Score.from128 a/ns) :: Vertex2 GLfloat)
                                                vertex (Vertex2 x1 (y+Score.from128 a/ns) :: Vertex2 GLfloat)

drawSeqs _ state = do
  (t,ms,db,s) <- atomically $ readTVar state
  matrixMode $= Modelview 0
  loadIdentity
  color (Color4 0.4 0.4 0.4 0.2 :: Color4 GLfloat)
  case s of
        Sq s ->   renderWordPOSWH 0.5 0.5 (s ^. _2) (s ^. _3) (1/15/8) (1/70) (s ^. _1)
        _ -> return ()
  let ns = realToFrac . fromIntegral $ length ms
  forM_ [0 .. ns -1] $ \nt -> do
                case db ^. dbseq (floor nt) of
                        Nothing -> return ()
                        Just s  -> do 
                                let y = nt/ns
                                forM_ [0..7] $ \p -> do
                                        let x' = p * 1/8
                                        renderPrimitive LineLoop $ do 
                                                color (Color4 0.6 0.6 0.6 0.3 :: Color4 GLfloat)
                                                vertex (Vertex2 (x') (y) :: Vertex2 GLfloat)
                                                vertex (Vertex2  (x' + 1/8) (y):: Vertex2 GLfloat)
                                                color (Color4 0.4 0.6 0.6 0.3 :: Color4 GLfloat)
                                                vertex (Vertex2 (x' + 1/8) (y+1/ns) :: Vertex2 GLfloat)
                                                vertex (Vertex2 (x') (y+1/ns) :: Vertex2 GLfloat)
                                        case s ^. at (floor p) of
                                                Just (Score.Pattern np wp sp)  -> do 
                                                        forM_ (zip (ap zip tail [0,1/24..]) [(0.3,np),(0.5,wp),(0.7,sp)]) $ \((x1,x2), (c,a)) -> do
                                                                color (Color4 0.4 0.4 0.4 1 :: Color4 GLfloat)
                                                                renderNumberPosWH 0.5 0.5 (realToFrac x'+ realToFrac x2 - 1/15/4) (realToFrac y + 1/200) (1/100) (1/80) a
                                                                renderPrimitive LineLoop $ do 
                                                                        color (Color4 c 0.8 0.8 0.3 :: Color4 GLfloat)
                                                                        vertex (Vertex2 (x' + x1) (y) :: Vertex2 GLfloat)
                                                                        vertex (Vertex2  (x' + x2) (y):: Vertex2 GLfloat)
                                                                        color (Color4 c 0.9 0.9 0.3 :: Color4 GLfloat)
                                                                        vertex (Vertex2 (x' + x2) (y+1/ns) :: Vertex2 GLfloat)
                                                                        vertex (Vertex2 (x' + x1) (y+1/ns) :: Vertex2 GLfloat)
                                                                renderPrimitive Quads $ do 
                                                                        color (Color4 (c + 0.2) 1 0.9 1 :: Color4 GLfloat)
                                                                        vertex (Vertex2 (x' + x1) (y) :: Vertex2 GLfloat)
                                                                        vertex (Vertex2  (x' + x2) (y):: Vertex2 GLfloat)
                                                                        color (Color4 c 1 0.6 1 :: Color4 GLfloat)
                                                                        vertex (Vertex2 (x' + x2) (y+Score.from128 a/ns) :: Vertex2 GLfloat)
                                                                        vertex (Vertex2 (x' + x1) (y+Score.from128 a/ns) :: Vertex2 GLfloat)
                                                                renderPrimitive Quads $ do 
                                                                        color (Color4 (c + 0.2) 0.8 0.9 0.1 :: Color4 GLfloat)
                                                                        vertex (Vertex2 (x' + x1) (y + 1/ns) :: Vertex2 GLfloat)
                                                                        vertex (Vertex2  (x' + x2) (y + 1/ns):: Vertex2 GLfloat)
                                                                        color (Color4 c 0.9 1 0.1 :: Color4 GLfloat)
                                                                        vertex (Vertex2 (x' + x2) (y+Score.from128 a/ns) :: Vertex2 GLfloat)
                                                                        vertex (Vertex2 (x' + x1) (y+Score.from128 a/ns) :: Vertex2 GLfloat)


                                                Nothing ->  do 
                                                        let x' = p * 1/8
                                                        forM_ (ap zip tail (take 4 [0,1/24..])) $ \((x1,x2)) -> do
                                                                renderPrimitive LineLoop $ do 
                                                                        color (Color4 0.8 0.8 0.8 0.3 :: Color4 GLfloat)
                                                                        vertex (Vertex2 (x' + x1) (y) :: Vertex2 GLfloat)
                                                                        vertex (Vertex2  (x' + x2) (y):: Vertex2 GLfloat)
                                                                        color (Color4 0.9 0.9 0.9 0.3 :: Color4 GLfloat)
                                                                        vertex (Vertex2 (x' + x2) (y+1/ns) :: Vertex2 GLfloat)
                                                                        vertex (Vertex2 (x' + x1) (y+1/ns) :: Vertex2 GLfloat)

