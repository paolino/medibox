import GL
import Connects
import OpenGlDigits
import Control.Concurrent.STM

import Graphics.UI.Gtk hiding (Point)
import Graphics.UI.Gtk.OpenGL 
import Graphics.Rendering.OpenGL 
import qualified Data.IntMap as IM
import Control.Monad





data CSynth = CSynth 
        {       _csi :: Int
        ,       _csname :: String
        ,       _csparams :: [String]
        }
someSynths = map (CSprite 0 (0.5,0.5) (0.1,0.1) . BSprite) 
        [       CSynth 0 "PIPPO" ["AMPL","FEEDB","LATCH","RETARD"] 
        ,       CSynth 0 "PLUTO" ["AMPL","RELEASE","FREQ","ALIGN"] 
        ,       CSynth 0 "MIMMO" ["AMPL","FEEDB","LEFTBIAS","SUSTAIN"] 
        ,       CSynth 0 "CAINO" ["AMPL","ATTACK","LATCH","PITCH"] 
        ,       CSynth 0 "ABELE" ["AMPL","FEEDB","TRIM","RATE","RAMP","RING"] 
        ]


instance Sprite CSynth where
        connections (CSynth i n ps) =  
		(map (\x -> Input x False)  . zip (repeat 1.04) $  [0.5 * h + fromIntegral i * h | i <-  [0 .. length ps]])  ++
		(map (\x -> Input x True)  . zip (repeat (-0.04)) $  [0.5 * h + fromIntegral i * h | i <-  [0 .. length ps]]) where
		h = 1/fromIntegral (length ps  + 1)
	edgeColor p  (CSynth _ _ ps)  i = kcolor p 1 (0.3 * fromIntegral (i `mod` (length ps + 1))) (0.2 * fromIntegral (i `mod` (length ps + 1))) 1
        renderSprite p c@(CSynth i n ps) = do
			let	h = 1/(fromIntegral $ length ps + 1) 
			 	pn n = (p  + n) / (n+1)
                        renderPrimitive LineLoop $ do
				mcolor p 2 3 4 1
                                vertex (Vertex2 0 0 :: Vertex2 GLfloat)
                                vertex (Vertex2 1 0 :: Vertex2 GLfloat)
				mcolor p 4 3 2 1
                                vertex (Vertex2 1 1 :: Vertex2 GLfloat)
                                vertex (Vertex2 0 1 :: Vertex2 GLfloat)
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

main = do
  initGUI
  bootGL
  window <- windowNew
  onDestroy window mainQuit
  set window [ containerBorderWidth := 8,
                   windowTitle := "tracks widget" ]
  hb <- hBoxNew False 1

  ref <- newTVarIO  $ Graph 
	(IM.fromList . zip [0..] $ someSynths)
        IM.empty

  connects <- graphing ref 
  set window [containerChild := connects] 
  widgetShowAll window
  dat <- widgetGetDrawWindow $ window
  cursorNew Tcross >>= drawWindowSetCursor dat . Just 
  mainGUI
