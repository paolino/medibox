
{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DataKinds, ViewPatterns,GADTs #-}


module Rythms where


import Control.Concurrent.STM
import Control.Concurrent

import Graphics.UI.Gtk hiding (Point,Signal, Object)
import Graphics.UI.Gtk.OpenGL 
import Graphics.Rendering.OpenGL  hiding (Projection)
import qualified Data.Map as M
import Control.Monad
import Sprite.Logic
import Sprite.Widget
import Control.Lens hiding (set)
import Haskell
import Data.Monoid
import Data.List.Zipper (insert,empty, cursor)

import OpenGlDigits
import GUI


data Synth = Pattern (M.Map Int GLfloat) | Synth Int  deriving Show

type instance SocketName Synth = String 


basePattern  = Object 
		(M.singleton 0 (SInput (-0.1,0.5) (0.5,0.5) ["pattern"]))
		(M.singleton 0 (SOutput (1.1,0.5) (0.5,0.5) "pattern" ))
		(Pattern $ M.fromList $ zip [0..7] $ repeat 0)

baseSynth n = Object
		(M.singleton 0 (SInput (-0.1,0.5) (0.5,0.5) ["pattern"]))
		M.empty 		
		(Synth n)

		
scrollSynth :: ScrollDirection -> Point -> Synth -> STM Synth
scrollSynth s (x,_) e@(Pattern y) 
	| x >=0 && x < 1 = let m = floor $ x * (fromIntegral $ M.size y)
			in return . Pattern $ at m  %~ fmap f $ y
	| otherwise = return e
	where f x = case s of
		ScrollUp ->  min 1 $ x + 1/128
		ScrollDown -> max 0 $ x - 1/128
		_ -> x
scrollSynth s (_,_) (Synth y) = return . Synth $ case s of
		ScrollUp ->  min 127 $ y + 1
		ScrollDown -> max 0 $ y - 1


setSynth :: Point -> Synth -> STM Synth
setSynth (x,y) e@(Pattern q) 
	| x >=0 && x < 1 && y >= 0 && y < 1 = let m = floor $ x * fromIntegral (M.size q)
			in return . Pattern $ at m  .~ Just (realToFrac y) $ q
	| otherwise = return e
setSynth _ x = return x



rbe (SInput (realToFrac -> x,realToFrac -> y) c _) = renderPrimitive LineLoop $ do
		vertex (Vertex2 (x - 0.05) (y - 0.05) :: Vertex2 GLfloat)
		vertex (Vertex2 (x + 0.05) (y - 0.05) :: Vertex2 GLfloat)
		vertex (Vertex2 (x + 0.05) (y + 0.05) :: Vertex2 GLfloat)
		vertex (Vertex2 (x - 0.05) (y + 0.05) :: Vertex2 GLfloat)
rbe (SOutput (realToFrac -> x,realToFrac -> y) c _ ) = do 
		renderPrimitive LineLoop $ do
			vertex (Vertex2 (x - 0.05) (y - 0.05) :: Vertex2 GLfloat)
			vertex (Vertex2 (x + 0.05) (y - 0.05) :: Vertex2 GLfloat)
			vertex (Vertex2 (x + 0.05) (y + 0.05) :: Vertex2 GLfloat)
			vertex (Vertex2 (x - 0.05) (y + 0.05) :: Vertex2 GLfloat)
  

renderSynth :: Object Synth -> IO ()
renderSynth (Object is os (Pattern  y))  = do
			let 	n = M.size y
			let 	d = 1 / fromIntegral n
			polygonSmooth $= Enabled
			forM_ (M.assocs y) $ \(i,v') -> do
				let 	x1 = d * fromIntegral i + d/4
					x2 = d * (fromIntegral i + 1) - d/4
					v = v'*0.7
				color (Color4 0.3 0.4 0.5 0.3:: Color4 GLfloat)
				renderNumberPosWH 0.5 0.5 (x2 - 2*d/10) (0.8) (1/8/fromIntegral n) (0.1) $ floor $ v' * 100
				
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

			forM_ (M.elems is) rbe
			forM_ (M.elems os) rbe

				
renderSynth (Object is os (Synth n))  = do
			polygonSmooth $= Enabled
			lineSmooth $= Enabled
			color (Color4 0.6 0.7 0.8 0.1:: Color4 GLfloat)
			renderNumberPosWH 0.5 0.5 (0.9) (0.5) (1/20) (1/10) $ n
			-- renderNumberPosWH 0.5 0.5 0.1 0.5 (1/20) (1/10) $ fromIntegral (v M.! 0)
			color (Color4 0.4 0.9 0.9 0.1:: Color4 GLfloat)
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
			forM_ (M.elems is) rbe
			forM_ (M.elems os) rbe



graph :: Graph Synth
graph = Graph (M.fromList $ 
	[ (0,(Affine (0.5,0.5) (0.1,0.06),basePattern))
	, (1,(Affine (0.5,0.5) (0.06,0.1),baseSynth 0))
	]) M.empty M.empty

main = do
	ref <- newTVarIO (insert graph empty)
	run setSynth scrollSynth renderSynth ref
