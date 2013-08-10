{-# LANGUAGE ExistentialQuantification, TypeFamilies, GADTs #-}
module Realize where

import Sequenza
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Control.Monad
import Sound.OSC
import System.IO

data Event a = Event 
        { _etime :: Tempo
        , _what :: a
        } deriving (Show,Read)



class Realize t where
        realize :: IntMap Presence -> t Projection -> [Event (t Double)]


instance Realize SSample where 
        realize pres (SSample vo pi sa ob) = let 
                vos = project pres vo
                pis = project pres pi
                f _ [] _ = []
                f pi ((tvo,vo):vos) [] = Event tvo (SSample vo  (0.5 + 1.5*pi) sa ob): f pi vos []
                f pi' ((tvo,vo):vos) ((tpi,pi):pis) 
                                | tvo < tpi = Event tvo (SSample vo (0.5 + 1.5*pi) sa ob): f pi' vos ((tpi,pi):pis)
                                | otherwise = f pi ((tvo,vo):vos) pis
                in case pis of
                        [] -> []
                        pis -> f (snd $ last pis) vos pis
instance Realize SControllo where
        realize pres (SControllo ef par va) = map (\(t,v) -> Event t (SControllo ef par v)) $ project pres va 

class Play a where 
        type Ctx a 
        play :: Ctx a -> Event a -> IO ()

data Playable where 
        Playable ::  (Realize t, Play (t Double))  => (IO (Ctx (t Double)), t Projection) -> Playable

data Globals = Globals 
        { _delay :: Tempo
        , _period :: Tempo
        , _subdivision :: Int
        }


output :: (Tempo, Tempo) -> (Tempo -> Tempo) -> IntMap Presence -> [Playable] -> IO () 
output (t1,t2) ft pres es = do
        forM_ es $ \(Playable (ctx,e)) -> do
                let rs = realize pres e 
                forM_ rs $ \(Event t x) ->  do
                        when (t < t2 && t >= t1) $ do
                                c <- ctx
                                play c $ Event (ft t) x

outputCycle :: Tempo -> IO Globals -> IO (IntMap Presence, [Playable]) -> IO ()
outputCycle t0 g f = do 
        Globals del per sub <- g
        let     dl = 1 / fromIntegral sub
                ts = [0, dl ..]
        forM_ [0..sub - 1] $ \n -> do
                sleepThreadUntil (t0 + per * fromIntegral n * dl)
                (pres,pls) <- f
                output ((!! n) . ap zip tail $ ts)  (\t -> t0 + del + per * t) pres pls
        outputCycle (t0 + per) g f

class Direct a where
        type DCtx a 
        direct :: DCtx a -> a -> IO ()

data Directable where
        Directable :: Direct a => (DCtx a, a) -> Directable

directControl :: IO Directable -> IO ()
directControl f = f >>= \(Directable (j,a)) -> direct j a >> directControl f



