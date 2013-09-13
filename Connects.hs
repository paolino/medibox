{-# LANGUAGE TemplateHaskell, DeriveFunctor, ExistentialQuantification, TypeFamilies, GeneralizedNewtypeDeriving, TypeOperators  #-}
module Sprite.Logic where


import Data.List
import Data.Ord
import qualified Data.Map as M
import Control.Arrow

import Control.Monad
import Control.Monad.Trans
import Data.Typeable
import Control.Lens hiding (set)
	
import Haskell






-- | Coloured placeable existential Sprite box
data CSprite =  CSprite 
	{	_cspriteColorPower :: GLfloat
	,	_cspriteTranspose :: Point 
	,	_cspriteScale :: Point 
	,	_cspriteSprite :: BSprite
	}

$(makeLenses ''CSprite)


csplace :: CSprite  -> Point -> Point
csplace (CSprite _ c k _)  x =  c .+. ((x .-. (0.5,0.5)) .*. k)

halfmove :: GLfloat -> Point -> CSprite -> CSprite
halfmove k  c s = cspriteTranspose .~ (mid k c $ s ^. cspriteTranspose) $ s 




-- | Links , polymorphic on the sprite with an index to the connection point


type a :¢ b = a (b)

placeSocket :: CSprite -> Socket -> Socket
placeSocket m = socketPoint %~ csplace m

 
sockets :: CSprite ->  M.Map (Indx Socket) Socket
sockets  m@(CSprite _ _ _ (BSprite s)) = M.map (placeSocket m) $ connections s

-- | Actual socket 
socket :: CSprite  -> Indx Socket -> Socket
socket m i = sockets m M.! i
-- | Actual point of a CSprite socket
center :: CSprite  -> Indx Socket -> Point
center m i = socket m i ^. socketPoint

type Versus = Bool

socketVersus :: CSprite -> Indx Socket -> Versus
socketVersus m i = center m i < m ^. cspriteTranspose 

-- | Distance from the csprite
spriteDistance :: Point -> CSprite  -> GLfloat
spriteDistance  c s = distance c $ s ^. cspriteTranspose 

-----------------------------------------------------------------------
data Edge a = Edge 
	{ _edgeStart :: (a,Indx Socket)
	, _edgeEnd ::  (a,Indx Socket) 
	} deriving Functor


-- | Database of sprites and links
data Graph = Graph
	{	_vertexes :: M.Map (Indx CSprite) CSprite
	,	_edges :: M.Map (Indx :¢ Edge Int) (Edge :¢ Indx CSprite)
	,	_assigned :: M.Map (Indx CSprite, Indx Socket) (Maybe TypeRep)
	}

$(makeLenses ''Graph)

querySocket :: Graph -> (Indx CSprite, Indx Socket) -> (Versus, Point, Maybe TypeRep)
querySocket g (ci,si) = (socketVersus m si, center m si, (g ^. assigned) M.! (ci,si)) where
	m = (g ^. vertexes) M.! ci

realEdges ::  Graph  ->  M.Map (Indx :¢ Edge Int) (Edge CSprite)
realEdges (Graph sps es _) = M.map (fmap (sps M.!)) es

edgeCenter ::  Edge CSprite -> Point
edgeCenter (Edge (e1 ,i1) (e2, i2)) = mid 1 (center e1 i1) (center e2 i2)

nearestEdges ::  Point -> Graph -> [Indx :¢ Edge Int]

nearestEdges c g = map snd . sortBy (comparing fst) . map (\(n,x) -> (distance c $ edgeCenter x,n)) . M.assocs $ realEdges g 

nearestSockets ::  Point -> Graph -> [(Indx CSprite, Indx Socket)]
nearestSockets c g = map snd . sortBy (comparing fst) $ do
			(n,m) <- M.assocs $ g ^. vertexes 
			(i,c') <- M.assocs . fmap (^. socketPoint) $ sockets m
			return  (distance c c',(n,i))




