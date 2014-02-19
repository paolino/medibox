{-# LANGUAGE TypeFamilies #-}

module Sequencer where

import Sound.OSC (Time,time,pauseThreadUntil)
import Control.Monad (ap)
import Debug.Trace

import Samples

-- time difference
type Period = Time

-- find next beat from 0 time
quantize :: Period -> IO Time
quantize pe = (\dt ->  (fromIntegral . ceiling $ dt/pe) * pe) `fmap` time

type family Params a
type family State a

-- fraction of a measure
type Beat = (Integer,Integer)

-- actual subperiod
fromBeat :: Period -> Beat -> Period 
fromBeat q (n,k) = (fromIntegral n * q / fromIntegral k)

quantizeBeat q (n,k) = let 
	d = q / fromIntegral k
	in (+ (d * (fromIntegral n - 1))) `fmap` quantize d
-- sequencer interface
data Feedback a = Feedback {
		params :: Params a
	, 	reset :: State a
	,	late :: Beat
	,	measure :: Period
	}

-- A sequencer asks a callback at regular intervals
newtype Sequencer a = Sequencer ((Time -> Time -> State a -> IO (Feedback a)) -> IO (Sequencer a))


-- step the state leaking a period subdivision
type Step a = State a -> Params a -> (Beat, State a)


-- make a sequencer from a concurrent sequencing environment
sequencer :: Step a -> Feedback a -> Sequencer a
sequencer fpr  feed = 
	let g (Feedback ps v0 l q) v f  = do
		let	(be,v') = fpr v ps
		t <- time
		nt <- quantizeBeat q be
		nq <- quantize q
		let 	(t',v'') =   if nq > nt then (nt,v') else (nq,v0)
			t'' = fromBeat q l + t'
		feed <- f  t'' (t' - t) v''
		pauseThreadUntil t'
		return . Sequencer $ g feed v''
	in Sequencer $ g feed (reset feed)



