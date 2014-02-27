

{-# LANGUAGE TypeFamilies, TemplateHaskell, Rank2Types #-}

module Euclidean.Elements (Element (..), runElements) where

import Graphics.UI.Gtk hiding (Point, Socket, Object, get, focus)
import Graphics.UI.Gtk.OpenGL hiding (Sink,get)
import Graphics.Rendering.OpenGL hiding (Sink, get)

import Data.List.PointedList
 
import Data.Maybe (catMaybes)
import Data.List (sortBy)
import Data.Ord

import Control.Monad (liftM2)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TVar,newTVarIO)
import qualified Data.Map as M
-- import Sprite.Widget ()
import Sprite.Logic -- (Affine (..), SocketName, Socket (..),Object (..),Graph (..))
import Sprite.GUI
import Data.Binary
import Euclidean.Data

import Control.Lens
import Sound.OSC

import Event

data Element = Pattern Euclidean | Sound  Instrument | Generator Double

instance  Binary Element where
        put (Pattern x) = put 'a' >> put x 
        put (Sound x) = put 'b' >> put x 
        put (Generator x) = put 'c' >> put x 
        get = do
                n <- get
                case n of 
                        'a' -> Pattern `fmap` get 
                        'b' -> Sound `fmap` get 
                        'c' -> Generator `fmap` get 


type instance SocketName Element = String



renderElement :: Object Element -> IO ()
renderElement (Object is os (Pattern _))  = do
			polygonSmooth $= Enabled
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
renderElement (Object is os (Sound _))  = do
			polygonSmooth $= Enabled
			color (Color4 1 0.9 1 0.8:: Color4 GLfloat)
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

renderElement (Object is os (Generator _))  = do
			polygonSmooth $= Enabled
			color (Color4 1 1 0.3 0.4:: Color4 GLfloat)
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

basePattern  = Object 
		(M.singleton 0 (SInput (-0.1,0.5) (0.5,0.5) ["pattern"]))
		(M.singleton 0 (SOutput (1.1,0.5) (0.5,0.5) "pattern" ))
		(Pattern (Euclidean [] 0 4 0))

baseSound = Object
		(M.singleton 0 (SInput (-0.1,0.5) (0.5,0.5) ["pattern"]))
		M.empty 		
		(Sound (Instrument 0.5 50 True $ M.fromList $ zip [0..] $ replicate 24 0))
baseGenerator = Object 
                M.empty
		(M.singleton 0 (SOutput (1.1,0.5) (0.5,0.5) "pattern"))
                (Generator 2)

baseGraph :: Graph Element
baseGraph = Graph (M.fromList $ 
		[ (0,(Affine (0.5,0.5) (0.1,0.06),basePattern))
		, (1,(Affine (0.5,0.5) (0.06,0.1),baseSound))
		, (2,(Affine (0.5,0.5) (0.1,0.1),baseGenerator))
		]) M.empty M.empty


type ZGE = PointedList (Graph Element)


runElements :: IO (TVar ZGE)
runElements =  do
        tg <- newTVarIO $ singleton baseGraph 
        forkIO $ run (const return) (\_ _ -> return) renderElement tg
        return tg
        

selectGenerator :: M.Map IObject Element -> IO (IObject,Time,Time)
selectGenerator m = do
        t <- time        
        let     k (i,Generator pe) = Just (i,t,quantize 0 pe t)
                k _ = Nothing
        return $ head . sortBy (comparing $ view  _3) . catMaybes . map k $ M.assocs m

effes :: ZGE -> M.Map IObject Element
effes zg = fmap (view $ _2 . object) $ zg ^. focus . vertexes

nextTick :: ZGE -> IO (Event IObject)
nextTick g = do 
        (i,dt,t) <- selectGenerator (effes g)
        pauseThreadUntil t >> return (Event i t dt)

