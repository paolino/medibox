{-# LANGUAGE ViewPatterns, GADTs, FlexibleContexts  #-}

module Sprite.Widget where


import Control.Arrow

import Graphics.UI.Gtk hiding (Point, Socket)
import Graphics.UI.Gtk.OpenGL 
import Control.Monad
import Control.Concurrent.STM
import Control.Monad.Trans
import Graphics.Rendering.OpenGL 
	
import GL
import Sprite.Logic

toGLfloat :: Double -> GLfloat
toGLfloat = realToFrac

renderAGL :: (a -> IO ()) -> RenderObject a IO 
renderAGL f (Object _ _ x)  (Affine (toGLfloat -> cx,toGLfloat -> cy) (toGLfloat -> sx,toGLfloat -> sy)) = do
	preservingMatrix $ do
		translate (Vector3 cx cy 0)
		scale sx sy 1
		preservingMatrix $ do 
			translate (Vector3 (-0.5) (-0.5) (0 :: GLfloat))
			f x

renderEdgeGL :: RenderEdge a IO 
renderEdgeGL (Edge (SOutput p1 c1 _) (SInput p2 c2 _)) = do
   renderPrimitive Points $ return () -- bug ??!?
   let	 v1 = fst (c1 .-. p1) > 0 
	 v2 = fst (c2 .-. p2) > 0
	 (x,y) = (toGLfloat *** toGLfloat) p1 :: (GLfloat, GLfloat)
	 (x1,y1) = (toGLfloat *** toGLfloat) p2 :: (GLfloat, GLfloat)	
   let x' = if v1 && v2 then [min x x1 - 0.1]  else 
		if not v1 &&  not v2 then [max x x1 + 0.1] else
			if x < (x1 - 0.1) && not v1 then [(x1+x)/2 + 0.05,(x1 + x)/ 2 - 0.05] else
				  if x1 < (x - 0.1) && not v2 then [(x1+x)/2 -0.05 ,(x1 + x)/ 2 + 0.05] else
					if  not v1 then  [x + 0.1,x1 - 0.1] else
						[x - 0.1, x1 + 0.1] 
   m <- newMap1 (0, 1)  $ 
	[ Vertex3 x y 0] ++
	( do 
		x <- x'
		y' <- [y,y1]
		[Vertex3 x y' 0]
	) ++
	[	Vertex3 x1 y1 0 
	]
   map1 $= Just (m :: GLmap1 Vertex3 GLfloat)
   let linear x y i = x + i * (y - x)
	
   renderPrimitive LineStrip $ forM_ [0 :: GLfloat ,1/60 .. 1] $ \i -> do
	-- color (Color4 (linear p1 p2 i) (linear p1 p2  i) (linear p1 p2 i) 0.1 :: Color4 GLfloat)
        evalCoord1 i

graphing :: Eq (SocketName a) => (a -> IO ()) ->  TVar (Graph a)  -> IO GLDrawingArea
graphing  renderA ref = do
  connecting <- newTVarIO Nothing
  coo <- newTVarIO (0,0)
  size <- newTVarIO  (1,1)
  lineSmooth $= Enabled

  connects <- mkCanva size . const $  do
	  g <- atomically $ readTVar ref
	  c <- atomically $ readTVar coo
	  conn <- atomically $ readTVar connecting
	  renderGraph renderEdgeGL (renderAGL renderA)  $ maybe g ($ c) conn
  widgetSetEvents connects [AllEventsMask]
  on connects buttonPressEvent $ do
	b <- eventButton
	cl <- eventClick
	ms <- eventModifier
	liftIO . atomically $ do
		
		g  <- readTVar ref 
		c  <- readTVar coo
		case cl of
			TripleClick -> return ()
			DoubleClick -> return () 
			SingleClick -> 	case b of
				LeftButton -> writeTVar connecting . Just $ moveVertex c g
				RightButton -> writeTVar connecting $ newEdge c g 
				MiddleButton -> writeTVar connecting $ modifyEdge c g  
				_ -> return ()
		return True
  on connects buttonReleaseEvent $ do
	b <- eventButton
	liftIO . atomically $ do
		g <- readTVar ref 
		c <- readTVar coo
		f <- readTVar connecting 
		case b of 
			_ -> case f of 
				Just f -> do 
					writeTVar ref $ f c
					writeTVar connecting Nothing
				Nothing -> return ()

	return True
  on connects motionNotifyEvent $ do
        (x,y) <- eventCoordinates
	liftIO . atomically $ do
		(wg,hg) <- readTVar size
		writeTVar coo (realToFrac x/fromIntegral wg,realToFrac (fromIntegral hg - y)/fromIntegral hg)
	return True
  return connects			
			

