{-# LANGUAGE TypeFamilies #-}
import GL
import OpenGlDigits
import Control.Concurrent.STM

import Graphics.UI.Gtk hiding (Point)
import Graphics.UI.Gtk.OpenGL 
import Graphics.Rendering.OpenGL  hiding (Projection)
import qualified Data.Map as M
import Control.Monad
import Sprite.Logic
import Sprite.Widget
import Control.Lens hiding (set)
import Haskell



data Synth = Pattern Int (M.Map Int GLfloat) | Projection Int (M.Map Int GLfloat) | Synth String

type instance SocketName Synth = String 


basePattern = Object 
		(M.singleton 0 (SInput (-0.1,0.5) (0.5,0.5) ["patternOut"]))
		(M.singleton 0 (SOutput (1.1,0.1) (0.5,0.5) "patternOut"))
		(Pattern 3 $ M.fromList $ zip [0..2] $ repeat 0)

baseProjection = Object 
		(M.singleton 0 (SInput (-0.1,0.5) (0.5,0.5) ["patternOut"]))
		(M.singleton 0 (SOutput (1.1,0.1) (0.5,0.5) "projectionOut"))
		(Projection 6 $ M.fromList $ zip [0..5] $ repeat 0)

baseSynth n x = Object
	(M.fromList $ [(fromIntegral i,SInput (-0.1,fromIntegral j / fromIntegral n) (0.5,0.5) ["projectionOut"]) | (i,j)  <- zip [0..n-1] [0..n-1]])
	M.empty
	(Synth x)

scrollSynth :: ScrollDirection -> Point -> Synth -> Synth
scrollSynth s (x,_) e@(Pattern n y) 
	| x >=0 && x < 1 = let m = floor $ x * fromIntegral n
			in Pattern n $ at m  %~ fmap f $ y
	| otherwise = e
	where f x = case s of
		ScrollUp ->  min 1 $ x + 1/128
		ScrollDown -> max 0 $ x - 1/128
		_ -> x
scrollSynth s (x,_) e@(Projection n y) 
	| x >=0 && x < 1 = let m = floor $ x * fromIntegral n
			in Projection n $ at m  %~ fmap f $ y
	| otherwise = e
	where f x = case s of
		ScrollUp ->  min 1 $ x + 1/128
		ScrollDown -> max 0 $ x - 1/128
		_ -> x
scrollSynth s (x,_) y = y

setSynth :: Point -> Synth -> Synth
setSynth (x,y) e@(Pattern n q) 
	| x >=0 && x < 1 && y >= 0 && y < 1 = let m = floor $ x * fromIntegral n
			in Pattern n $ at m  .~ Just (realToFrac y) $ q
	| otherwise = e
setSynth (x,y) e@(Projection n q) 
	| x >=0 && x < 1 && y >= 0 && y < 1 = let m = floor $ x * fromIntegral n
			in Projection n $ at m  .~ Just (realToFrac y) $ q
	| otherwise = e
setSynth _ x = x


graph :: Graph Synth
graph = Graph (M.fromList $ 
	[ (0,(Affine (0.5,0.5) (0.06,0.1),basePattern))
	, (1,(Affine (0.5,0.5) (0.12,0.1),baseProjection))
	, (2, (Affine (0.5,0.5) (0.1,0.2),baseSynth 5 "SAMPLER")) 
	]) M.empty M.empty
       

renderSynth :: Synth -> IO ()
renderSynth (Pattern n y) = do
			let 	d = 1 / fromIntegral n
			polygonSmooth $= Enabled
			forM_ (M.assocs y) $ \(i,v') -> do
				let 	x1 = d * fromIntegral i + d/4
					x2 = d * (fromIntegral i + 1) - d/4
					v = v'*0.7
				color (Color4 0.3 0.4 0.5 0.3:: Color4 GLfloat)
				renderNumberPosWH 0.5 0.5 (x2 - 2*d/10) (0.8) (1/8/fromIntegral n) (0.1) $ floor $ v' * 128
				
				color (Color4 0.6 0.7 0.8 0.1:: Color4 GLfloat)
				renderPrimitive Quads $ do
					-- mcolor p 2 3 4 1
					vertex (Vertex2 x1 0.05 :: Vertex2 GLfloat)
					vertex (Vertex2 x2  0.05 :: Vertex2 GLfloat)
					-- mcolor p 4 3 2 1
					vertex (Vertex2 x2 (0.05 + v) :: Vertex2 GLfloat)
					vertex (Vertex2 x1 (0.05 + v) :: Vertex2 GLfloat)
			color (Color4 0.8 0.9 1 0.1:: Color4 GLfloat)
			forM_ [(0.1,0.1), (0.9,0.1),(0.9,0.9),(0.1,0.9)] $ \(xc,yc) -> 
				renderPrimitive Polygon $ forM_ [0,0.1.. 2*pi] $ \a -> do
					let 	x = xc + 0.1*cos a
					 	y = yc + 0.1*sin a
					vertex (Vertex2 x y:: Vertex2 GLfloat)

			color (Color4 0.8 0.9 1 0.1:: Color4 GLfloat)
			renderPrimitive Quads $ do
                                vertex (Vertex2 0.1 0 :: Vertex2 GLfloat)
                                vertex (Vertex2 0.9 0 :: Vertex2 GLfloat)
                                vertex (Vertex2 0.9 1 :: Vertex2 GLfloat)
                                vertex (Vertex2 0.1 1 :: Vertex2 GLfloat)
			renderPrimitive Quads $ do
                                vertex (Vertex2 0 0.1 :: Vertex2 GLfloat)
                                vertex (Vertex2 1 0.1 :: Vertex2 GLfloat)
                                vertex (Vertex2 1 0.9 :: Vertex2 GLfloat)
                                vertex (Vertex2 0 0.9 :: Vertex2 GLfloat)
renderSynth (Projection n y) = do
			let 	d = 1 / fromIntegral n
			polygonSmooth $= Enabled
			forM_ (M.assocs y) $ \(i,v') -> do
				let 	x1 = d * fromIntegral i + d/4
					x2 = d * (fromIntegral i + 1) - d/4
					v = v'*0.7
				color (Color4 0.3 0.4 0.5 0.3:: Color4 GLfloat)
				renderNumberPosWH 0.5 0.5 (x2 - 2*d/10) (0.8) (1/8/fromIntegral n) (0.1) $ floor $ v' * 128
				
				color (Color4 0.6 0.7 0.8 0.1:: Color4 GLfloat)
				renderPrimitive Quads $ do
					-- mcolor p 2 3 4 1
					vertex (Vertex2 x1 0.05 :: Vertex2 GLfloat)
					vertex (Vertex2 x2  0.05 :: Vertex2 GLfloat)
					-- mcolor p 4 3 2 1
					vertex (Vertex2 x2 (0.05 + v) :: Vertex2 GLfloat)
					vertex (Vertex2 x1 (0.05 + v) :: Vertex2 GLfloat)
			color (Color4 0.9 1 0.8 0.1:: Color4 GLfloat)
			forM_ [(0.1,0.1), (0.9,0.1),(0.9,0.9),(0.1,0.9)] $ \(xc,yc) -> 
				renderPrimitive Polygon $ forM_ [0,0.1.. 2*pi] $ \a -> do
					let 	x = xc + 0.1*cos a
					 	y = yc + 0.1*sin a
					vertex (Vertex2 x y:: Vertex2 GLfloat)

			renderPrimitive Quads $ do
                                vertex (Vertex2 0.1 0 :: Vertex2 GLfloat)
                                vertex (Vertex2 0.9 0 :: Vertex2 GLfloat)
                                vertex (Vertex2 0.9 1 :: Vertex2 GLfloat)
                                vertex (Vertex2 0.1 1 :: Vertex2 GLfloat)
			renderPrimitive Quads $ do
                                vertex (Vertex2 0 0.1 :: Vertex2 GLfloat)
                                vertex (Vertex2 1 0.1 :: Vertex2 GLfloat)
                                vertex (Vertex2 1 0.9 :: Vertex2 GLfloat)
                                vertex (Vertex2 0 0.9 :: Vertex2 GLfloat)
				
renderSynth (Synth n) = do
			polygonSmooth $= Enabled
			lineSmooth $= Enabled
			color (Color4 0.6 0.7 0.8 0.1:: Color4 GLfloat)
			renderWordPOSWH 0.5 0.5 (0.1) (0.5) (1/20) (0.1) $ n
			color (Color4 1 0.9 0.8 0.1:: Color4 GLfloat)
			forM_ [(0.1,0.1), (0.9,0.1),(0.9,0.9),(0.1,0.9)] $ \(xc,yc) -> 
				renderPrimitive Polygon $ forM_ [0,0.1.. 2*pi] $ \a -> do
					let 	x = xc + 0.1*cos a
					 	y = yc + 0.1*sin a
					vertex (Vertex2 x y:: Vertex2 GLfloat)

			renderPrimitive Quads $ do
                                vertex (Vertex2 0.1 0 :: Vertex2 GLfloat)
                                vertex (Vertex2 0.9 0 :: Vertex2 GLfloat)
                                vertex (Vertex2 0.9 1 :: Vertex2 GLfloat)
                                vertex (Vertex2 0.1 1 :: Vertex2 GLfloat)
			renderPrimitive Quads $ do
                                vertex (Vertex2 0 0.1 :: Vertex2 GLfloat)
                                vertex (Vertex2 1 0.1 :: Vertex2 GLfloat)
                                vertex (Vertex2 1 0.9 :: Vertex2 GLfloat)
                                vertex (Vertex2 0 0.9 :: Vertex2 GLfloat)
			{-
			let	h = 1/(fromIntegral $ length ps + 1) 
			 	pn n = (p  + n) / (n+1)

			forM_ (zip [0,h..] [n]) $ \(d,n) ->  do
				mcolor p 1 1 2 1
				renderWordPOSWH 0.5 0.5 (1/5) (d + h/4) (1/20) (h/2) $ take 15 n
				renderPrimitive Quads $ do
					mcolor p 5 20 30 0.1 
					vertex (Vertex2 0 d :: Vertex2 GLfloat)
                                	vertex (Vertex2 1 d :: Vertex2 GLfloat)
					mcolor p 15 20 30 0.1 
                                	vertex (Vertex2 1 (d + h) :: Vertex2 GLfloat)
                                	vertex (Vertex2 0 (d + h) :: Vertex2 GLfloat)
			forM_ (zip [h,2*h..] (ps)) $ \(d,n) -> do
				color $ Color4 (pn 2) (pn 2) (pn 2) 1 
				renderWordPOSWH 0.5 0.5 (1/5) (d + h/4) (1/20) (h/2) $ take 15 n
				renderPrimitive Quads $ do
					mcolor p 20 30 5 0.1 
					vertex (Vertex2 0 d :: Vertex2 GLfloat)
                                	vertex (Vertex2 1 d :: Vertex2 GLfloat)
					mcolor p 20 30 15 0.1 
                                	vertex (Vertex2 1 (d + h) :: Vertex2 GLfloat)
                                	vertex (Vertex2 0 (d + h) :: Vertex2 GLfloat)
			color $ Color4 1 (pn 1) (pn 1) 1
                        forM_ (map socketPoint $ connections c) $ \(x,y) -> renderPrimitive LineLoop $ do
				let 	x0 = x + 0.03
				 	x1 = x - 0.03
				 	y1 = y - 0.03
				 	y0 = y + 0.03
				mcolor p 4 3 2 1
                                vertex (Vertex2 x0 y0 :: Vertex2 GLfloat)
                                vertex (Vertex2 x1 y0 :: Vertex2 GLfloat)
				mcolor p 5 3 1 1
                                vertex (Vertex2 x1 y1 :: Vertex2 GLfloat)
                                vertex (Vertex2 x0 y1 :: Vertex2 GLfloat)
			-}
main = do
  initGUI
  bootGL
  window <- windowNew
  onDestroy window mainQuit
  set window [ containerBorderWidth := 8,
                   windowTitle := "tracks widget" ]
  hb <- hBoxNew False 1

  ref <- newTVarIO  graph 

  connects <- graphing setSynth scrollSynth renderSynth ref 
  set window [containerChild := connects] 
  widgetShowAll window
  dat <- widgetGetDrawWindow $ window
  cursorNew Tcross >>= drawWindowSetCursor dat . Just 
  mainGUI
