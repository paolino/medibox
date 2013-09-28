{-# LANGUAGE ViewPatterns, GADTs, FlexibleContexts  #-}

module Sprite.Widget where


import Control.Arrow ((***))

import Graphics.UI.Gtk hiding (Point, Socket, Object)
import Graphics.UI.Gtk.OpenGL hiding (Sink)
import Control.Monad
import Control.Concurrent.STM
import Control.Monad.Trans
import Graphics.Rendering.OpenGL hiding (Sink)

import Data.List.Zipper
	
import GL
import Sprite.Logic

toGLfloat :: Double -> GLfloat
toGLfloat = realToFrac

renderAGL :: (Object a -> IO ()) -> RenderObject a IO 
renderAGL f x (Affine (toGLfloat -> cx,toGLfloat -> cy) (toGLfloat -> sx,toGLfloat -> sy)) = do
	preservingMatrix $ do
		translate (Vector3 cx cy 0)
		scale sx sy 1
		preservingMatrix $ do 
			translate (Vector3 (-0.5) (-0.5) (0 :: GLfloat))
			f x 

renderEdgeGL :: RenderEdge a IO 
renderEdgeGL (Edge (SOutput p1 c1 _ _) (SInput p2 c2 _)) = do
   renderPrimitive Points $ return () -- bug ??!?
   color (Color4 0.3 0.4 0.5 1 :: Color4 GLfloat)
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

addGraph ref x = modifyTVar ref $ insert x . head . dropWhile (not . beginp) . iterate pop

graphing 
	:: Eq (SocketName a) 
	=> (Point -> a -> STM a) 
	-> (ScrollDirection -> Point -> a -> STM a) 
	-> (Object a  -> IO ()) 
	->  TVar (Zipper (Graph a))
	-> IO GLDrawingArea
graphing  innerclick innerscroll renderA ref = do
  let saferight x = let 
	x' = right x
	in if endp x' then  x else x'
  connecting <- newTVarIO Nothing
  coo <- newTVarIO (0,0)
  size <- newTVarIO  (1,1)
  lineSmooth $= Enabled

  connects <- mkCanva size . const $  do
	  g <- fmap cursor . atomically $ readTVar ref
	  c <- atomically $ readTVar coo
	  conn <- atomically $ readTVar connecting
	  case conn of 
		Nothing -> renderGraph renderEdgeGL (renderAGL renderA) g
		Just f -> atomically (f c) >>= renderGraph renderEdgeGL (renderAGL renderA)  
  widgetSetEvents connects [AllEventsMask]
  on connects buttonPressEvent $ do
	b <- eventButton
	cl <- eventClick
	ms <- eventModifier
	liftIO . atomically $ do
		
		g  <- cursor `fmap` readTVar ref 
		c  <- readTVar coo
		case cl of
			TripleClick -> return ()
			DoubleClick -> return () 
			SingleClick -> 	case b of
				LeftButton -> case Control `elem` ms of
					True -> addGraph ref (deleteEdge c g)
					False -> case Shift `elem` ms of
						False -> writeTVar connecting . Just $ return . moveVertex c g
						True -> writeTVar connecting . Just $ \c -> sendToVertex c g innerclick
				RightButton -> writeTVar connecting $ fmap (fmap return) $ newEdge c g 
				MiddleButton -> writeTVar connecting $ fmap (fmap return) $ modifyEdge c g  
				_ -> return ()
		return True
  on connects scrollEvent $ do
	d <- eventScrollDirection
	ms <- eventModifier
	liftIO . atomically $ do 
		g <- cursor `fmap`  readTVar ref 
		c  <- readTVar coo
		let f = case d of 
			ScrollUp -> (* 1.1)
			ScrollDown -> (/1.1)
		case ms == [Control] of
			True -> addGraph ref $ scaleYVertex c g f
			False -> case ms == [Shift] of 
				True -> sendToVertex c g (innerscroll d) >>= addGraph ref 
				False -> addGraph ref . flip (scaleXVertex c) f $ scaleYVertex c g f
			
	return True
  on connects keyPressEvent $ do
	v <- eventKeyVal
	ms <- eventModifier
	liftIO . atomically $ do
		c  <- readTVar coo
		g  <-  cursor `fmap` readTVar ref 
		case keyName v of
			"c" ->  case cloneVertex c g of
					Nothing -> return ()
					Just f ->  writeTVar connecting . Just $ return . f
			"d" -> addGraph ref $ deleteVertex c g
			
			"z" -> case ms of
					[Control] -> modifyTVar ref saferight		
					_ -> return ()
			"y" -> case ms of
					[Control] -> modifyTVar ref left
					_ -> return ()
			_ -> return ()
	return True
  on connects keyReleaseEvent $ do
	v <- eventKeyVal
	liftIO . atomically $ do
		c  <- readTVar coo
		g  <-  cursor `fmap` readTVar ref 
		f <- readTVar connecting 
		case keyName v of
			_ -> case f of 
				Just f -> do 
					f c >>= addGraph ref 
					writeTVar connecting Nothing
				Nothing -> return ()
	return True

  on connects buttonReleaseEvent $ do
	b <- eventButton
	liftIO . atomically $ do
		g <- cursor `fmap`  readTVar ref 
		c <- readTVar coo
		f <- readTVar connecting 
		case b of 
			_ -> case f of 
				Just f -> do 
					f c >>= addGraph ref 
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
			

