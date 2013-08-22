{-# LANGUAGE TemplateHaskell#-}  

module Score (Tempo, Pattern (..), Sequenza,  pnumber, pwidth, pshift, Score, score, Projection (..), ampl, offset, shift, quant, cutin, project) where

import Data.List (sort) 
import qualified Data.IntMap as IM (elems)
import Data.IntMap (IntMap)
import Control.Arrow (second, (***))

import Control.Lens 
import Data.Binary

-------------------------------
import Haskell



type Tempo = Double

data Pattern = Pattern 
        { _pnumber :: Int
        , _pwidth :: Int
        , _pshift :: Int
        }

$(makeLenses ''Pattern)

instance Binary Pattern where
  put (Pattern a b c) = put a >> put b >> put c
  get = get >>= \a -> get >>= \b -> get >>= \c -> return (Pattern a b c)


type Sequenza = IntMap Pattern  

-- | A Score is the result of a Sequenza computation
type Score a = [(Tempo, a)]


score :: Sequenza -> [Tempo]
score seq =  sort . concat $ every (IM.elems seq) baseEvents where
        baseEvents (Pattern n w s) = every [from128 s + 1 / fromIntegral w / fromIntegral n * fromIntegral j | j <- [0 .. n - 1]] (`floatMod` 1)


data Projection = Projection
        { _ampl :: Int
        , _offset :: Int
        , _shift :: Int
        , _quant :: Int
        , _cutin :: Int
        } 

$(makeLenses ''Projection)


instance Binary Projection where
  put (Projection a b c d e) = put a >> put b >> put c >> put d >> put e
  get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> return (Projection a b c d e)

project :: [Tempo] -> Projection -> Score Double
project sc (Projection da o sh qm ci') = let 
                        ci = from128 ci'
                        ts = hystogram (fromIntegral qm) $ zip sc (repeat 1)
                        cut x = if x >= ci then x else 0
                        sht t = floatMod (from128 sh + t) 1
                        tsc ts = filter ((> 0) . snd) . every ts $ second cut
                        m = maximum $ map snd ts
                        in tsc . every ts $ sht *** ((from128 o +).(from128 da *). (/fromIntegral m) . fromIntegral)




 




