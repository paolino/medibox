module Sequenza where
{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, ViewPatterns, FlexibleInstances, Rank2Types #-}       
import Control.Concurrent.STM
import Control.Concurrent
import Data.List hiding (find)
import Sound.OSC
import Sound.SC3 hiding (pitch)
import qualified Data.Map as M
import System.FilePath
import System.FilePath.Find
import Control.Arrow
import System.Random.Shuffle
import Data.Monoid        
import Control.Monad
import System.Console.Haskeline 
import System.Random
import System.IO

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Exception as AlsaExc

import System.Environment (getArgs, )

import Debug.Trace

import qualified Sound.ALSA.Sequencer.Connect as Connect

import qualified System.Exit as Exit
import qualified System.IO as IO
import Control.Concurrent.STM
import Control.Lens hiding (inside)
import Control.Lens.Tuple
import Language.Haskell.TH.Lens
import Data.Ord

correct' xs@(x:_) ys = let 
        (ms,us) = partition ((== x) . fst)  ys
        in case ms of 
                [] -> True
                [(_,y)] ->  not (y `elem` xs) && correct' (y:xs) us
                _ -> False

correct x ys = correct' [x] ys

floatMod x y = let
        r = x/y
        in r - fromIntegral (floor r) * y

inoctave x 
        | x >= 0.5 && x <= 2 = x
        | x > 2 = inoctave (x/2)
        | x < 0.5 = inoctave (x * 2)
pitchmap = reverse (take 64 $ iterate ( inoctave . (/1.5)) 1) ++ (take 64 $ iterate (inoctave . (*1.5)) 1)

every = flip map

from128 x = 1/128*fromIntegral x
from128p x = from128 x - 0.5
type Tempo = Double

data Pat = Pat 
        { _pnumber :: Int
        , _pwidth :: Int
        , _pshift :: Int
        }
        
        deriving (Show,Read)

$(makeLenses ''Pat)
      
 

data Presence = Base (M.Map Int Pat) | Higher (M.Map Int Pat) deriving (Show,Read)

baseEvents :: Pat -> [Tempo]
baseEvents (Pat n w s) = every [from128 s + 1 / fromIntegral w / fromIntegral n * fromIntegral j | j <- [0 .. n - 1]] (`floatMod` 1)

higherEvents :: M.Map Int Presence -> Pat -> [(Tempo,Int)]
higherEvents mp (Pat n w s) = case M.lookup n mp of
        Nothing ->  []
        Just p -> map (_1 %~ (\x -> (from128 s + x / fromIntegral w) `floatMod` 1)) $ schedule mp p



collapse m (x,_) (y,_) = abs (x - y) < 1/m
quantize m = (/m) . fromIntegral . floor . (*m) 
stat m = map (fst . head &&& sum . map snd) . groupBy (collapse m) . map (first $ quantize m)

schedule :: M.Map Int Presence -> Presence -> [(Tempo,Int)]
schedule _ (Base pre) = case sort . concat $ every (M.elems pre) baseEvents  of
        [] -> []
        rs -> flip zip (repeat 1) rs

schedule mp (Higher pre) = case sort . concat $ every (M.elems pre) (higherEvents mp)  of
        [] -> []
        rs ->  rs

data Seq = Seq 
        { _pres :: Presence
        , _sample :: Int
        , _pitch :: Int -- pitch
        , _damp :: Int
        , _shift :: Int
        , _quant :: Int
        }
        deriving (Show,Read)

$(makeLenses ''Seq)

noseq l = Seq 
        (l (M.fromList $ zip [0..7] $ repeat (Pat 0 1 0)))
        0
        64
        64
        0
        32


sequenza ::  M.Map Int Presence -> Seq -> [(Tempo,Double)]
sequenza mp (Seq p s pi da sh qm) = let 
        ts = stat (fromIntegral qm) $ schedule mp p
        m = maximum (map snd ts)
        in every ts $ \(t,n) ->  (floatMod (fromIntegral sh / fromIntegral qm + t) 1,from128 da * fromIntegral n/fromIntegral m)

nolens  = lens (\_ -> 0) (\s _ -> s)

male n = lens f g where
        f (Base m) = m M.! n
        f (Higher m) = m M.! n
        g (Base m) v = Base (M.insert n v m)
        g (Higher m) v = Higher (M.insert n v m)

seq_lenses = 
        [  pres . male 0 . pnumber
        ,  pres . male 1 . pnumber
        ,  pres . male 2 . pnumber
        ,  pres . male 3 . pnumber
        ,  pres . male 4 . pnumber
        ,  pres . male 5 . pnumber
        ,  pres . male 6 . pnumber
        ,  pres . male 7 . pnumber
        ,  pres . male 0 . pwidth
        ,  pres . male 1 . pwidth
        ,  pres . male 2 . pwidth
        ,  pres . male 3 . pwidth
        ,  pres . male 4 . pwidth
        ,  pres . male 5 . pwidth
        ,  pres . male 6 . pwidth
        ,  pres . male 7 . pwidth
        ,  pres . male 0 . pshift
        ,  pres . male 1 . pshift
        ,  pres . male 2 . pshift
        ,  pres . male 3 . pshift
        ,  pres . male 4 . pshift
        ,  pres . male 5 . pshift
        ,  pres . male 6 . pshift
        ,  pres . male 7 . pshift
        , sample, pitch, shift, damp, quant
        ]

