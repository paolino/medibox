{-# LANGUAGE ExistentialQuantification, TypeFamilies, GADTs, FlexibleContexts #-}
module Realize where

import Sequenza
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Control.Monad
import Sound.OSC
import System.IO
import Control.Arrow


data Event a = Event 
        { _etime :: Time
        , _event :: a
        }        
        deriving (Show,Read)

type ScoreH = Score (Double, Double -> Double)

holdFromScore :: Score a -> Double -> a
holdFromScore ys x = snd . last . takeWhile ((< (x + 1)) . fst) . concat . iterate (map $ first (+1)) $ ys



type QueryScore m = Int -> m (Score Double)

class Realize t where
        type  EventR t 
        realize :: forall m . Monad m => QueryScore m -> t -> m [Event (EventR t)]


class Play a where 
        type Ctx a 
        play :: Ctx a -> Event a -> IO ()

data Playable where 
        Playable ::  (Show t, Realize t, Play (EventR t))  => (IO (Ctx (EventR t)), t) -> Playable

data Globals = Globals 
        { _delay :: Tempo
        , _period :: Tempo
        , _subdivision :: Int
        }


output :: (Tempo, Tempo) -> (Tempo -> Tempo) -> (Int -> IO (Score Double)) -> [Playable] -> IO () 
output (t1,t2) ft pres es = do
        forM_ es $ \(Playable (ctx,e)) -> do
                rs <- realize pres e
                forM_ rs $ \(Event t x) ->  do
                        when (t < t2 && t >= t1) $ do
                                c <- ctx
                                play c $ Event (ft t) x

outputCycle :: Tempo -> (Int -> IO (Score Double)) -> IO Globals -> IO [Playable] -> IO ()
outputCycle t0 ioscore g f = do 
        Globals del per sub <- g
        let     dl = 1 / fromIntegral sub
                ts = [0, dl ..]
        forM_ [0..sub - 1] $ \n -> do
                sleepThreadUntil (t0 + per * fromIntegral n * dl)
                pls <- f
                output ((!! n) . ap zip tail $ ts)  (\t -> t0 + del + per * t) ioscore pls
        outputCycle (t0 + per) ioscore g f




