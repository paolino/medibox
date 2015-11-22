
{-# LANGUAGE TemplateHaskell, ViewPatterns #-}

module Board where



import Control.Lens
import Control.Lens.TH
import Sound.OSC



data E a = E {
  _event :: a,
  _timet :: Time
  } deriving (Show,Read)

makeLenses ''E

dFloor x s = (s *) . fromIntegral . floor . (/s) $ x
dMod t s = t - (t `dFloor` s) 
dNorm = flip dMod 1
  
data Linear = Linear Double Double

project :: Linear -> Time -> Time
project (Linear a b ) t = dNorm (t * a + b)

projectE :: Linear -> E a -> E a
projectE l e = over timet (project l) e

data Span = Span Time Time

x `inSpan` Span ((<= view timet x) -> True) ((> view timet x) -> True) =  True
_ `inSpan` _ = False

type Board a = [E a]

pickBoard s = map (view event) . filter (flip inSpan s . over timet dNorm)


