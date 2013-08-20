{-# LANGUAGE TemplateHaskell, ExistentialQuantification, TypeFamilies, GADTs, FlexibleContexts #-}
module Realize where

import Sequenza
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Control.Monad
import Sound.OSC
import System.IO
import Control.Arrow
import Control.Lens


type Track = (Int,Int)

data Realize a = Realize {
        _track :: Track
        , _what :: a
        } deriving (Show,Read)

$(makeLenses ''Realize)
type QueryScore m = Track -> m (Score Double)


class Play a where 
        type Ctx a 
        play :: Ctx a -> (Tempo , (Double,a)) -> IO ()


data Playable where 
        Playable ::  (Show a, Play a)  => (IO (Ctx a), a) -> Playable

data Globals = Globals 
        { _delay :: Tempo
        , _period :: Tempo
        , _subdivision :: Int
        }

realize :: (Functor m, Monad m) => QueryScore m -> Realize a -> m (Score  (Double,a))
realize f (Realize t x) =  map (second (\v -> (v,x))) `fmap` f t

output :: (Tempo, Tempo) -> (Tempo -> Tempo) -> QueryScore IO  -> [Realize Playable] -> IO () 
output (t1,t2) ft pres es = do
        forM_ es $ \(Realize tr (Playable (ctx,e))) -> do
                rs <- realize pres (Realize tr e)
                forM_ rs $ \(t, x) ->  do
                        when (t < t2 && t >= t1) $ do
                                c <- ctx
                                play c $ (ft t, x)

outputCycle :: Tempo -> QueryScore IO -> IO Globals -> IO [Realize Playable] -> IO ()
outputCycle t0 ioscore g f = do 
        Globals del per sub <- g
        let     dl = 1 / fromIntegral sub
                ts = [0, dl ..]
        forM_ [0..sub - 1] $ \n -> do
                sleepThreadUntil (t0 + per * fromIntegral n * dl)
                pls <- f
                output ((!! n) . ap zip tail $ ts)  (\t -> t0 + del + per * t) ioscore pls
        outputCycle (t0 + per) ioscore g f




