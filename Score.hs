{-# LANGUAGE TemplateHaskell#-}  

-- | This module exposes the basic definitions for what we call score. A 'Score' is a list of associatons between a normalized time and a normalized value, a cloud of points in [0,1] x [0,1]. 
-- To populate time we use a set of 'Pattern', each being a regular spread of points in [0,1]. 
-- The 'Projection' data contains informations to regularize the patterns contributions.
-- Shift and width are common to both 'Pattern' and 'Projection'. Both operations are always made modulo 1.
-- 'Projection' also takes care of quantization , amplification, offset and cutin filtering.
-- The tuple (Sequenza,Projection) is what is called a track which can be projected into a 'Score' by 'project' function. The 'score' function is exported for cache optimization.
-- Both structures are a collection of Ints inteded inside the 0 .. 127 value to directly map to midi limited expression.

module Score (Tempo, Pattern (..), Sequenza,  pnumber, pwidth, pshift, Score, tempi, 
        Projection (..), prampl, proffset, prwidth, prshift, prquant, prcutin, project, from128) where

import Data.List (sort) 
import qualified Data.IntMap as IM (elems)
import Data.IntMap (IntMap)
import Control.Arrow (second, (***))

import Control.Lens (makeLenses) 
import Data.Binary (Binary (..))
import Data.Default (Default (..))

-------------------------------
import Haskell (hystogram, floatMod, every, normalize)


from128 x = 1/128 * fromIntegral x
from128p d x = exp (d* (from128 x - 0.5))

type Tempo = Double

-- | Basic contribution for score definition
data Pattern = Pattern 
        { _pnumber :: Int -- ^ number of equispaced points
        , _pwidth :: Int  -- ^ distance modulation
        , _pshift :: Int  -- ^ shift modulation
        }

$(makeLenses ''Pattern)

instance Default Pattern where
	def = Pattern 8 64 0

instance Binary Pattern where
  put (Pattern a b c) = put a >> put b >> put c
  get = get >>= \a -> get >>= \b -> get >>= \c -> return (Pattern a b c)


-- | An Int indexed collection of patterns
type Sequenza = IntMap Pattern  



tempi :: Sequenza -> [Tempo]
tempi seq =  concat $ every (IM.elems seq) baseEvents where
        baseEvents (Pattern n w s) = every [from128 s + 1 * from128p 4  w / fromIntegral n * fromIntegral j | j <- [0 .. n - 1]] (`floatMod` 1)


-- | Filter definition for sequenza computation
data Projection = Projection
        { _prampl :: Int
        , _proffset :: Int
        , _prwidth :: Int
        , _prshift :: Int
        , _prquant :: Int
        , _prcutin :: Int
        } 

$(makeLenses ''Projection)

instance Default Projection where
	def = Projection 64 0 64 0 32 0

instance Binary Projection where
  put (Projection a b c d e f) = put a >> put b >> put c >> put d >> put e >> put f
  get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> get  >>= \f -> return (Projection a b c d e f)


-- | A Score is the result of a sequenza computation after projection
type Score = [(Tempo, Double)]

-- | project a sequenza acccording to a Projection into a Score
project :: [Tempo] -> Projection -> Score
project sc (Projection da o prw sh qm ci) = let 
                        wt = from128p 4 prw
                        sht t = floatMod (from128 sh + wt *  t) 1
                        (xs,ys) = unzip $  hystogram (fromIntegral qm) $ map sht sc 
                        in filter ((> from128 ci) . snd) . zip xs $ map (((from128 o +).(from128 da *))) . normalize $ map fromIntegral ys







 




