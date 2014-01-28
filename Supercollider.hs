{-# LANGUAGE TypeFamilies, FlexibleInstances, NoMonomorphismRestriction #-}

module Supercollider where

import Control.Arrow ((&&&))
import qualified Data.Map as IM
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad (forM_,when)

import Sound.OSC
import Sound.SC3 hiding (pitch)
import System.FilePath
import System.FilePath.Find
import System.Directory
import Control.Monad
import Data.Maybe
import System.Random
import Data.List


collapse 0 = 0
collapse 1 = 0
collapse 2 = 2
collapse 3 = 3
collapse 4 = 3
collapse 5 = 5
collapse 6 = 7
collapse 7 = 7
collapse 8 = 8
collapse 9 = 7
collapse 10 = 12
collapse 11 = 12
modcollapse x = let
        (o,n) = x `divMod` 12
        in o * 12 + collapse n


divModFloat x y= let 
        r = fromIntegral (floor $ x / y)
        in (r , x - r * y)

withSC3n :: Int -> Connection UDP a -> IO a
withSC3n i = withTransport (openUDP "127.0.0.1" $ fromIntegral i)

servers = [57110]
freqs y = [440 * (1.059463)^^x | x <- map (subtract 48 . (+y) . modcollapse) [0 .. 300]]

quantf _ [] = 0
quantf x ((y',y''):rs) 
	  | x <= y' && x - y' > y'' - x  = y''
	  | x <= y'  = y'
	  | otherwise = quantf x  rs

quantizefreq x = quantf x $ zip `ap` tail $ freqs 40
quantizefreq' f x = quantf x $ zip `ap` tail $ freqs f


quantize2 k y x = let 
        xn = [takeWhile (<5) . dropWhile (< 0.2) $ sort [3 ^^ x / 2 ^^ y| x <- [-kn .. kn], y <- [-12 .. 12]] | kn <- [0 .. 6]]
        in quantf x $ zip `ap` tail $ map (*y) $ xn !! k
{-
(
SynthDef(\basso,{|freq=0.5,amp|
	var s1,r1,r2,zot,fu;
	fu = Lag.kr(freq,0.05);
	zot = LFSaw.ar(fu * (1 + ([l[0].kr,l[1].kr,l[2].kr,l[3].kr]/100)
	),pi*[z[0].kr,z[1].kr,z[2].kr,z[3].kr]);
	s1 = Mix.ar (SinOsc.ar (
		fu * (1 + (SinOsc.kr(20)/100)),
		((pi*zot*[p[0].kr,p[1].kr,p[2].kr,p[3].kr]/4)).mod(2*pi),
		0.5,
		0.1)
		,0);

	r1 = LPF.ar(s1,fu *12 ,1);
	r2 = HPF.ar(r1,fu*2,1);
	Out.ar(0,Pan2.ar(0.5*Lag.kr(amp,0.01)*r2))
	}
).add
)
-}

-- bootSin :: Int -> IO (Double -> Double -> Double -> Double ->  IO ())
bootSin j =  do 
        let     fu = 2 * lag (kR "freq") 0.05
                zot = lfSaw AR (fu * (1/2 + mce (map kR ["l1","l2","l3","l4"])/100)) (pi * mce (map kR ["z1","z2","z3","z4"]))
                s1 = mix 
                        (sinOsc AR 
                                (fu * (1 + mce (map kR ["p1","p2","p3","p4"])/64))
                                (pi / 16 * zot )
                                )
                s2 = 0.4 * sinOsc AR (fu/2) 0
                r1 = lpf (s1) (2500)
                r2 = hpf r1 (120)
                g = out 0 (0.6 * pan2 ((kR "amp") * r2) 0 1)
                m = d_recv (synthdef "bassoon" g)
                cm = s_new "bassoon" 1000 AddToTail 1 []
        _ <- withSC3 $ send (n_free [1000])
        withSC3 (send (withCM m cm))
	return $  \t p a r ha fb ps -> do
                withSC3n 57110 . sendBundle . bundle t . return $ 
                         n_set 1000  $ [("amp",a),("freq",quantize2 ha fb $ p)]++
                                zip ["l1","l2","l3","l4","z1","z2","z3","z4","p1","p2","p3","p4"] (IM.elems ps)

bootGrainFM j =  do 
        let     
                clk = impulse AR (kR "freq") 0
                base = kR "base" * 400 * (1 + (sinOsc AR 0.125 0 * kR "wipe"))
                modu = kR "mod" * 400 
                pan = sinOsc AR 2 0
		grain = grainFM 2 clk (kR "dur"/10) base modu (kR "pos" * 10) pan (-1) 512
                r1 = lpf grain (kR "lop" * 1000)
                r2 = hpf r1 (kR "hip" * 1000)
                g = out 0 (0.6 * pan2 ((kR "amp") * r2) 0 1)
                m = d_recv (synthdef "gfm" g)
                cm = s_new "gfm" 1000 AddToTail 1 []
        _ <- withSC3 $ send (n_free [1001])
        withSC3 (send (withCM m cm))
	return $  \t p a r ha fb ps -> do
                withSC3n 57110 . sendBundle . bundle t . return $ 
                         n_set 1001  $ [("amp",a),("freq",quantize2 ha fb $ p)]++
                                zip ["dur","base","mod","pos","wipe","lop","hip"] (IM.elems ps)



{-
(
SynthDef(\bass1,{|freq=0.5,dur=0.5,trate=0.5,sw=1,amp=0.5,pos=0.5,pos2=0.5,s1a=0.1,s2a=0.1,s3a=0.2,tag=0.5,det = 0,rq=1|

    var clk, pan,gen,env;
	var s1,s2,s3,s4;
    //clk = Impulse.ar(trate*100);
    pan = SinOsc.kr(2)/30;
	//env = Env.perc(0.001, 1, 1, -8) ;
	//gen = EnvGen.kr(env, gate, doneAction: 2);
	s1 = s1a * lfSaw.ar (freq * 160);
	s2 = s1a * lfSaw.ar (freq * 160 * (det/17 + 1));
	s3 = s2a * SinOsc.ar (freq * 80);
	s4 = s2a * SinOsc.ar (freq * 40);
	Out.ar(0,Pan2.ar(BLowPass.ar(s1 + s2 + s3  ,(0.25 + tag/2) * (freq * 160),rq),pan))
}).add;
)
-}
kR a = control KR a 0

bootBass :: Int -> IO (Double -> Double -> Double -> Double -> Int -> Double -> IM.Map Int Double -> IO ())
bootBass j =  do 
	n <- randomIO :: IO Int
        let     s1 = (kR "s1") * lfSaw AR (kR "pitch") 0 
                s2 = (kR "s2") *lfSaw AR (kR "pitch" * (1 + kR "det"/50)) 0
                s3 = (kR "s3") *sinOsc AR (kR "pitch"/2) 0
                bl = bLowPass (s1 + s2 + s3) (kR "tag" * 2 * (kR "pitch"))  (kR "rq" * 2)
                cp = compander (bl)(bl) (1) (1.0) (0.5) (0.01) (0.01)
	withSC3n j . send $ d_recv . synthdef ("bass1" ++ show n) . out 0 $   
		envGen KR 1 1 0 1 RemoveSynth (envTrapezoid  (kR "shape") (kR "skew") (control KR "rel" 0.4) 1)  * (control KR "amp" 0.2) 
                *       pan2 cp 0 1	
        return $  \t p a r ha fb ps -> do
			withSC3n 57110 . sendBundle . bundle t . return $ 
                         s_new ("bass1" ++ show n) (-1) AddToTail 1 $
                                [("amp",a),("pitch",quantize2 ha fb p),("rel",r)]++
                                zipWith  (\x (s,f) -> (s,f x))  (IM.elems ps) [("s1",id),("s2",id),("s3",id),("tag",id),("rq",id),("det",id)
                                        ,("skew",id),("shape",id)] 

bootSinM :: Int -> IO (Double -> Double -> Double -> Double -> Int -> Double -> IO ())
bootSinM j =  do 
        let u = 0.1 * sinOsc AR (control KR "pitch" 250 * 2) 0
	n <- randomIO :: IO Int
	withSC3n j . send $ d_recv . synthdef ("sinh" ++ show n) . out 0 $ mixN 2 $  
		0.7 * envGen KR 1 1 0 1 RemoveSynth (envPerc  0 (control KR "rel" 0.4)) *  (control KR "amp" 0.2) *mce  [
		sinOscFB AR (control KR "pitch" 250) 0 ,
		sinOscFB AR (control KR "pitch" 250 * 1.01) 0 
		]
		
	return $  \t p a r ha fb -> do
			withSC3n 57110 . sendBundle . bundle t . return $ 
                         s_new ("sinh" ++ show n) (-1) AddToTail 1 [("amp",a),("pitch",quantize2 ha fb p),("rel",r)]

bootSinH :: Int -> IO (Double -> Double -> Double -> Double -> Int -> Double -> IM.Map Int Double -> IO ())
bootSinH j =  do 
        let u = 0.1 * sinOsc AR (control KR "pitch" 250 * 2) 0
	n <- randomIO :: IO Int
	withSC3n j . send $ d_recv . synthdef ("sinh" ++ show n) . out 0 $ mixN 2 $  
		envGen KR 1 1 0 1 RemoveSynth (envPerc  0 (control KR "rel" 0.4)) *  (control KR "amp" 0.2) *mce  [
		sinOsc AR (control KR "pitch" 250) 0 ,
		sinOsc AR (control KR "pitch" 250 * 1.01) 0 
		]
		
	return $  \t p a r ha fb ps -> do
			withSC3n 57110 . sendBundle . bundle t . return $ 
                         s_new ("sinh" ++ show n) (-1) AddToTail 1 $ [("amp",a),("pitch",quantize2 ha fb p),("rel",r)]++
                                zipWith  (\x (s,f) -> (s,f x))  (IM.elems ps) [("s1",id),("s2",id),("s3",id),("tag",id),("rq",id)]


bootRisset :: Int -> IO (Double -> Double -> Double -> Double -> Int -> Double -> IO ())
bootRisset j = do 
        return $ \t p a r ha fb -> do
			withSC3n 57110 . sendBundle . bundle t . return $ 
                         s_new ("risset") (-1) AddToTail 1 [("amp",a),("pitch",quantize2 ha fb p),("rel",r)]

bootSample :: Int -> (Int,(FilePath,String)) -> IO ()
bootSample j (n,(fp,i)) = do
        withSC3n j . send $ b_free n
        withSC3n j . send $ b_allocRead n fp 0 0
        withSC3n j . send $ d_recv . synthdef i . out 0 $ envGen KR 1 1 0 1 RemoveSynth (envPerc 0 (control KR "rel" 0.1)) *  (control KR "amp" 0) * orig
        where orig =  playBuf 2 AR (fromIntegral n) (1 +  control KR "pitch" 0) 1 0 NoLoop RemoveSynth  


playSample  t p v r s = do
                withSC3n 57110 . sendBundle . bundle t . return $ 
                         s_new s (-1) AddToTail 1 $ [("amp",v),("pitch",p),("rel",r)] 
                               


initSynths :: [FilePath] -> IO (Int -> Double -> Double -> Double -> Double -> Int -> Double -> IM.Map Int Double -> IO (), [Int])
initSynths samples = do
        forM_ servers $ \i -> withSC3n i . send $ p_new [(1, AddToTail, 0)]
        -- ls <- map (id &&& takeBaseName)  `fmap` (find (depth ==? 0) (extension ==? ".wav") sampledir)
        let ls = map (id &&& takeBaseName) samples
        sequence_ $ do 
                j <- servers
                l <- zip [0..] ls
                return $ bootSample j l
	pluck <- bootSinH 57110
        zuck <- bootSin 57110
        frock <- bootGrainFM 57110
	let  sel _ _ _ _ 0 ha fb ps = return ()
	     sel _ _ 0 _ _ ha fb ps = return ()
             sel i t a p r ha fb ps
		| i == 7 = zuck  t (50 + p * 50) a r ha fb ps
		| i == 6 = pluck t  (50*4 + p * 50*4) a r ha fb ps 
		| i == 5 = pluck t (50*8 + p * 50*8) a r ha fb ps
		| i < 5 = do 
			t <- time
			playSample t p a r . snd . fromJust . lookup i . zip [0..] $ ls
	return $ (sel , map fst . zip [0..] $ ls)

newtype Sequencer = Sequencer ([(Int,Double,Double,Double,Double,Double,Double,IM.Map Int Double)] -> IO Sequencer)

noteOut :: [FilePath] -> IO (Sequencer,[Int])
noteOut s = do
	(f,is) <- initSynths s
	let g ts js  = do
		t <- time
		let 	(t':rs) = dropWhile (<=t) ts
		forM_ js $ \(i,p,a,d,c,ha,fb,wn) -> f i (t' + fromIntegral  (floor (c * 128))  * 0.12/2)  a p (d * 0.12 * 8) (floor $ ha * 6) 
                        (220 * normalize (scale fb)) wn
		t <- time
		when (t' > t) $ pauseThreadUntil t'
		return (Sequencer $ g rs)
	t0 <- time
	return $ (Sequencer $ g $ iterate (+ 0.12) t0,is)

scale fb 
        | fb >= 0.5 = 3 ^^ (floor $ (fb - 0.5)*12)
        | otherwise = 3 ^^ (floor $ (fb - 0.5)*12)

normalize x 
        | x > 2 = normalize (x/2)
        | x < 1 = normalize (x*2)
        | otherwise = x
