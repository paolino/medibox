{-# LANGUAGE TemplateHaskell, DeriveFunctor, ExistentialQuantification #-}
module Connects where


import Data.List
import Data.Ord
import qualified Data.IntMap as IM
import Control.Arrow

import Graphics.UI.Gtk hiding (Point)
import Graphics.UI.Gtk.OpenGL 
import Control.Monad
import Control.Concurrent.STM
import Control.Monad.Trans
import Graphics.Rendering.OpenGL 

import Control.Lens hiding (set)
	
import Haskell
import GL2D
import GL




type ColorPower = GLfloat

data Input = Input Point Bool
socketPoint (Input x _) = x

-- | Connectable , renderable sprites
class Sprite s where
	-- | enumerate all connecting point
	connections :: s -> [Input]
	renderSprite :: ColorPower -> s -> IO ()
	edgeColor :: ColorPower -> s -> Int -> Color4 GLfloat 

data BSprite = forall s . Sprite s => BSprite s

-- | Coloured placeable existential Sprite box
data CSprite =  CSprite ColorPower Point Point BSprite

csplace :: CSprite  -> Point -> Point
csplace (CSprite _ c k _)  x =  c .+. ((x .-. (0.5,0.5)) .*. k)

halfmove :: GLfloat -> Point -> CSprite -> CSprite
halfmove k  c s@(CSprite _ c' _ _) = move (mid k c c') s 

move c' (CSprite cl c t s) = CSprite cl c' t s

-- | Database of sprites and links
data Graph = Graph
	{	_vertexes :: IM.IntMap CSprite
	,	_edges :: IM.IntMap (Edge Int)
	}

-- spriteLens :: Functor f => Int -> (Maybe BSprite -> f (Maybe BSprite)) -> Graph -> f Graph
-- spriteLens 

-- | Links , polymorphic on the sprite with an index to the connection point
data Edge a = Edge (a,Int) (a,Int) deriving Functor

 
center :: CSprite  -> Int -> Point
center m@(CSprite _ c k (BSprite s)) i = csplace m (socketPoint $ connections s !! i)

socket :: CSprite  -> Int -> Input
socket m@(CSprite _ c k (BSprite s)) i = let 
	Input x v = connections s !! i
	in Input (csplace m x) v



spriteDistance :: Point -> CSprite  -> GLfloat
spriteDistance  c (CSprite _ c' _ _) = distance c' c

realEdges ::  Graph  -> [(Int,Edge CSprite)]
realEdges (Graph sps es) = map f (IM.assocs es) where
	f (n,Edge (ke1,i1) (ke2,i2)) =  (n, Edge (sps IM.! ke1, i1) (sps IM.! ke2,i2))

edgeCenter ::  Edge CSprite -> Point
edgeCenter (Edge (e1 ,i1) (e2, i2)) = mid 1 (center e1 i1) (center e2 i2)

nearestEdge ::  Point -> Graph -> Maybe Int
nearestEdge c g = case realEdges g of 
		[] -> Nothing
		rs -> Just $ fst . snd $ minimumBy (comparing fst) $ map (\(n,x) -> (distance c $ edgeCenter x,(n,x))) rs

nearestSprite ::  Point -> Graph -> Maybe (Int,Int)
nearestSprite c g = case IM.assocs $ _vertexes g of 
		[] -> Nothing
		rs -> Just $ snd $ minimumBy (comparing fst) $ map (\(n,i,c') -> (distance c c',(n,i))) $ do
			(n,m@(CSprite d c k (BSprite s))) <- rs
			(i,c) <- zip [0..] $ map (csplace m) $ map socketPoint $ connections s
			return (n,i,c)

nearestVersus ::  Point -> Graph -> Maybe Bool
nearestVersus c g = case IM.assocs $ _vertexes g of 
		[] -> Nothing
		rs -> Just $ snd $ minimumBy (comparing fst) $ map (\(c',v) -> (distance c c',v)) $ do
			(_,m@(CSprite d c k (BSprite s))) <- rs
			(c,v) <- map (\(Input x v) -> (csplace m x, v))  $ connections s 
			return (c,v)

nearestConnection ::  Point -> Graph -> Maybe (Int,Int,Int)
nearestConnection c g@(Graph sps es)  = let
		cs = do 
			(n,Edge (ke1,i1) (ke2,i2)) <- IM.assocs es
			e1 <- return $ sps IM.! ke1
			e2 <- return $ sps IM.! ke2
			let	c1 = distance c (center e1 i1)
				c2 = distance c (center e2 i2)
			[(c2,(n,ke1,i1)),(c1,(n,ke2,i2))]
		
		in case cs of 
			[] -> Nothing 
			cs -> Just . snd . minimumBy (comparing fst) $ cs



onecyc [] = []
onecyc (x:xs) = xs ++ [x]	

xor False False = True
xor True True  = True
xor _ _ = False 

renderCSprite :: CSprite -> IO ()
renderCSprite (CSprite d (cx,cy) (sx,sy) (BSprite s)) = do
	preservingMatrix $ do
		translate (Vector3 cx cy 0)
		scale sx sy 1
		preservingMatrix $ do 
			translate (Vector3 (-0.5) (-0.5) (0 :: GLfloat))
			renderSprite d s

renderCord :: Bool -> Bool -> Color4 GLfloat -> Color4 GLfloat -> Point -> Point -> IO ()
renderCord v1 v2 (Color4 r1 g1 b1 a1) (Color4 r2 g2 b2 a2)  (x,y) (x1,y1) = do
   renderPrimitive Points $ return ()
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
	
   renderPrimitive LineStrip $ forM_ [0,1/60 .. 1] $ \i -> do
	color (Color4 (linear r1 r2 i) (linear g1 g2  i) (linear b1 b2 i) (linear a1 a2 i) :: Color4 GLfloat)
        evalCoord1 i

renderEdge  :: ColorPower ->  Edge CSprite -> IO ()
renderEdge q (Edge (e1@(CSprite d1 _ _ (BSprite s1)) ,i1) (e2@(CSprite d2 _ _ (BSprite s2)) ,i2)) = renderPrimitive Lines $ do
	let Input (x1,y1) v1 = socket e1 i1
	    Input (x2,y2) v2 = socket e2 i2
	    c1 = edgeColor (q*d1) s1 i1
	    c2 = edgeColor (q*d2) s2 i2
	when (q == 0) $ print "ahi"
	renderCord v1 v2 c1 c2 (x1,y1) (x2,y2)

renderConnecting g@(Graph sps _) ((k,i),(x,y)) = renderPrimitive Lines $ do
	e@(CSprite d _ _ (BSprite s)) <- return $ sps IM.! k
	let 		Input (x0,y0) v0 = socket  e i
			v1 = maybe True id $ nearestVersus (x,y) g
	renderCord v0 v1 (edgeColor d s i) (Color4 0 0 0 0.1 :: Color4 GLfloat) (x0,y0) (x,y)


renderCanvas (x,y) = renderPrimitive Quads $ do
	mcolor 1 0 0 0 0.1
	vertex (Vertex2 0 0   :: Vertex2 GLfloat)
	mcolor 0.90 0 0 0 0.1
	vertex (Vertex2 x 0   :: Vertex2 GLfloat)
	mcolor 0.80 0 0 0 0.1
	vertex (Vertex2 x y   :: Vertex2 GLfloat)
	mcolor 0.90 0 0 0 0.1
	vertex (Vertex2 0 y   :: Vertex2 GLfloat)

	mcolor 1 0 0 0 0.1
	vertex (Vertex2 1 0   :: Vertex2 GLfloat)
	mcolor 0.90 0 0 0 0.1
	vertex (Vertex2 1 y   :: Vertex2 GLfloat)
	mcolor 0.80 0 0 0 0.1
	vertex (Vertex2 x y   :: Vertex2 GLfloat)
	mcolor 0.90 0 0 0 0.1
	vertex (Vertex2 x 0   :: Vertex2 GLfloat)

	mcolor 1 0 0 0 0.1
	vertex (Vertex2 1 1   :: Vertex2 GLfloat)
	mcolor 0.90 0 0 0 0.1
	vertex (Vertex2 1 y   :: Vertex2 GLfloat)
	mcolor 0.80 0 0 0 0.1
	vertex (Vertex2 x y   :: Vertex2 GLfloat)
	mcolor 0.90 0 0 0 0.1
	vertex (Vertex2 x 1   :: Vertex2 GLfloat)

	mcolor 1 0 0 0 0.1
	vertex (Vertex2 0 1   :: Vertex2 GLfloat)
	mcolor 0.90 0 0 0 0.1
	vertex (Vertex2 0 y   :: Vertex2 GLfloat)
	mcolor 0.80 0 0 0 0.1
	vertex (Vertex2 x y   :: Vertex2 GLfloat)
	mcolor 0.90 0 0 0 0.1
	vertex (Vertex2 x 1   :: Vertex2 GLfloat)

graphing ::  TVar Graph  -> IO GLDrawingArea
graphing  ref = do
  connecting <- newTVarIO Nothing
  moving <- newTVarIO Nothing
  rotating <- newTVarIO Nothing
  coo <- newTVarIO (0,0)
  size <- newTVarIO  (1,1)
  lineSmooth $= Enabled

  -- create the fixed dimension canvas with the drawing function for the expose events
  connects <- mkCanva size . const $  do
	  g@(Graph sps es) <- atomically $ readTVar ref
	  c <- atomically $ readTVar coo
	  let me = nearestEdge c g
	  atomically (readTVar connecting) >>= maybe (return ()) (renderConnecting g)
	  forM_ (realEdges g) $ \(i,e) -> do
		case me of
			Just ei -> if ei == i then lineWidth $= 2 else return ()
			Nothing -> return ()
		renderEdge 1 e
		lineWidth $=1 
	  forM_ (IM.elems sps) $ renderCSprite 
	  renderCanvas c

  widgetSetEvents connects [AllEventsMask]-- [PointerMotionMask, KeyPressMask, KeyReleaseMask, ScrollMask, Button1MotionMask, ButtonPressMask]
  on connects buttonPressEvent $ do
	b <- eventButton
	cl <- eventClick
	ms <- eventModifier
	liftIO . atomically $ do
		
		g@(Graph sps es)   <- readTVar ref 
		c <- readTVar coo
		case cl of
			TripleClick -> return ()
			DoubleClick -> return () 
			SingleClick -> 	case b of
				LeftButton -> do 
					modifyTVar moving $ \mc -> case mc of 
						Just n -> Just n
						Nothing ->  case  nearestSprite  c g of
							Just (n,i) -> Just n
							Nothing -> Nothing
				RightButton -> do
					if Control `elem` ms then 
						modifyTVar ref $ \g@(Graph sps es)  -> 
							case  nearestEdge  c g of 
								Just n -> Graph sps (IM.delete n es)
								_ -> g
					else do 
						g <- readTVar ref 
						modifyTVar connecting $ \mc -> 
							case mc of 
								Nothing -> case  nearestSprite  c g of 
									Just (n,i) -> Just ((n,i),c)
									Nothing -> Nothing
								Just x -> Just x
				MiddleButton -> do
					g@(Graph sps es)   <- readTVar ref 
					mc <- readTVar connecting 
					case mc of 
						Just _ -> return ()
						Nothing -> case  nearestConnection c g  of
							Nothing -> return ()
							Just (n,m,j) -> do 
								writeTVar ref $ Graph sps (IM.delete n es)
								writeTVar connecting $ Just ((m,j),c)
				_ -> return ()
		return True
  on connects buttonReleaseEvent $ do
	b <- eventButton
	liftIO . atomically $ do
		g@(Graph sps es)   <- readTVar ref 
		c <- readTVar coo
		case b of 
			LeftButton -> writeTVar moving Nothing
			_ -> do 	
				zm  <- readTVar connecting
				case zm of 
					Nothing -> return ()
					Just ((m,j),_) -> do 
						writeTVar connecting Nothing 
						case  nearestSprite  c g of 
							Just (n,i) -> case m == n of
								True -> return ()
								False -> writeTVar ref $ Graph sps (IM.insert (firstFree es) (Edge (m,j) (n,i)) es)
							Nothing -> return ()

	return True
  on connects scrollEvent $ do
	dir <- eventScrollDirection 
	mods <- eventModifier
	let f = case dir of 
		ScrollUp -> 1.1
		ScrollDown -> 0.9
	    xf = if Control `elem` mods then 1 else f

	liftIO . atomically $ do
		c <- readTVar coo
		g@(Graph sps es)   <- readTVar ref 
		case  nearestSprite  c g of
			Nothing -> return ()
			Just (n,_) -> do
				writeTVar ref $ flip Graph es $ IM.adjust (\(CSprite d c k s) -> CSprite d c ((xf,f) .*. k) s) n sps
	return True
  
  on connects keyPressEvent $ do
	key <- eventKeyVal 
	liftIO . atomically $ do	
		modifyTVar ref $ \g@(Graph sps es)  -> Graph sps (IM.fromList . zip [0..] .  onecyc . IM.elems $ es)
		c <- readTVar coo
		when (keyName key == "d") $ modifyTVar ref $ \g@(Graph sps es)  -> 
				case  nearestEdge  c g of 
					Just n -> Graph sps (IM.delete n es)
					_ -> g
		when (keyName key == "n") $ do
				g <- readTVar ref 
				modifyTVar connecting $ \mc -> 
					case mc of 
						Nothing -> case  nearestSprite  c g of 
							Just (n,i) -> Just ((n,i),c)
							Nothing -> Nothing
						Just x -> Just x
						

		when (keyName key == "m") $ do
				g@(Graph sps es)   <- readTVar ref 
				mc <- readTVar connecting 
				case mc of 
					Just _ -> return ()
					Nothing -> case  nearestConnection c g  of
						Nothing -> return ()
						Just (n,m,j) -> do 
							writeTVar ref $ Graph sps (IM.delete n es)
							writeTVar connecting $ Just ((m,j),c)
		when (keyName key == "space") $ do
			g <- readTVar ref
			modifyTVar moving $ \mc -> case mc of 
				Just n -> Just n
				Nothing ->  case  nearestSprite  c g of
					Just (n,i) -> Just n
					Nothing -> Nothing
	return True

  on connects keyReleaseEvent $ do
	key <- eventKeyVal 
	liftIO . atomically $ do
		c <-  readTVar coo
		g@(Graph sps es) <- readTVar ref 
		when (keyName key `elem` ["n","m"]) $ do 
				zm  <- readTVar connecting
				case zm of 
					Nothing -> return ()
					Just ((m,j),_) -> do 
						writeTVar connecting Nothing 
						case  nearestSprite  c g of 
							Just (n,i) -> case m == n of
								True -> return ()
								False -> writeTVar ref $ Graph sps (IM.insert (firstFree es) (Edge (m,j) (n,i)) es)
							Nothing -> return ()
		when (keyName key == "space") $ do
			writeTVar moving Nothing
	return True

  on connects motionNotifyEvent $ do
        (x,y) <- eventCoordinates
	liftIO . atomically $ do
		(wg,hg) <- readTVar size
        	let (x',y') =  (realToFrac x/fromIntegral wg,realToFrac (fromIntegral hg - y)/fromIntegral hg)
		modifyTVar connecting $ fmap $ \(n,_) -> (n,(x',y'))  
	        mc <- readTVar moving
		case mc of
			Nothing -> return ()
			Just n -> do
				Graph sps es <- readTVar ref
				writeTVar ref $ flip Graph es $ IM.adjust (halfmove 10 (x',y')) n sps


		Graph sps es <- readTVar ref
		writeTVar ref $ Graph (IM.map (\cs@(CSprite _ c k s) -> CSprite (spriteDistance (x',y') cs/2) c k s) sps) es
		writeTVar coo (x',y')
	return True
  return connects			
			


