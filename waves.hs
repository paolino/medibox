{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, ViewPatterns, FlexibleInstances #-}       
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

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Exception as AlsaExc

import System.Environment (getArgs, )


import qualified Sound.ALSA.Sequencer.Connect as Connect

import qualified System.Exit as Exit
import qualified System.IO as IO
import Control.Concurrent.STM
import Control.Lens hiding (inside)
import Control.Lens.Tuple
import Language.Haskell.TH.Lens
import Data.Ord

times :: Int -> [Double]
times l = take l $ [0,2*pi/fromIntegral l .. ]


sins ::  [(Double,Double,Double)] -> [Double -> Double]
sins = map (\(x,k,r) t -> r * (sin $ k + x * t)) 

wave  
      :: [Double] -- harmonics
      -> Int -- phase index
      -> Int -- power index
      -> Double -- time 
      -> Double -- value

wave xs i j t = sum $ map ($ t) (sins $ zip3 xs (phs !! i) (pws !! j)) where
        phs = map (randomRs (0,2*pi) . mkStdGen) [0 ..]
        pws = map (randomRs (0,1) . mkStdGen) [0 ..]

values  w l  i j c d a =  let
             qs = map (wave (map fromIntegral [1..c]) i j) (times l)
             rs = cycle qs
             mq = maximum $ map abs qs
             in case c <= 0 of
                        True -> [0,0..] 
                        False -> map (\x -> a + d * x/mq)  $ drop (w `mod` l) rs      
             

data W = W 
        { _dc :: Int
        , _width :: Int
        , _shift :: Int
        , _phindex :: Int
        , _pwindex :: Int
        , _harms :: Int
        , _damp :: Int
        } deriving (Show,Read)

$(makeLenses ''W)

dampW = (map ((1/) . (1.05^)) [128,127 .. 0] !!)
cutIn c = log (1 + (fromIntegral c + 1)/128)/ log 2

-- extrace a value of a wave given the loop width and an index fo the time in it
valueW  :: Int -- index
        -> W 
        -> [Double]
valueW w (W a 0 dw  i j c d) = [0,0..]
valueW w (W a l dw  i j c d) = values (w + dw) l i j c (dampW d) (fromIntegral a/128 - 0.5)
 


data Seq = Seq 
        { _dilatation :: Int -- dilatation
        , _phase :: Int
        , _sample :: W 
        , _presence :: W -- presence
        , _power :: W
        , _pitch ::W -- pitch
        , _groove ::W -- groove
        }
        deriving (Show,Read)

data Hype = Hype
        { _hype_dilatation :: Int
        , _hype_phase :: Int
        , _hype_set :: M.Map Int Seq
        , _hype_presence :: W
        , _hype_precedence :: W
        }
        deriving (Show,Read)
        
$(makeLenses ''Hype)
nolens = lens (\_ -> 0) (\s _ -> s)


$(makeLenses ''Seq)

seq_lenses = 
        [  nolens , power . dc , power . width, power . shift, power . phindex , power . pwindex, power . harms, power . damp
        ,  nolens ,pitch . dc, pitch . width, pitch . shift, pitch . phindex , pitch . pwindex, pitch . harms, pitch . damp
        , dilatation , groove . dc , groove . width, groove . shift, groove . phindex , groove . pwindex, groove . harms, groove . damp 
        ,  phase, presence . dc , presence . width, presence . shift, presence . phindex , presence . pwindex, presence . harms, presence . damp
        ,  nolens, sample . dc , sample . width, sample . shift, sample . phindex , sample . pwindex, sample . harms, sample . damp
        
        ]
hype_lenses = 
        [nolens,nolens, nolens, nolens, nolens, nolens, nolens, nolens, nolens, nolens, nolens, nolens, nolens, nolens, nolens, nolens,  
         hype_dilatation , hype_precedence . dc , hype_precedence . width, hype_precedence . shift, hype_precedence . phindex , 
         hype_precedence . pwindex, hype_precedence . harms, hype_precedence . damp , 
         hype_phase, hype_presence . dc , hype_presence . width, hype_presence . shift, hype_presence . phindex , 
         hype_presence . pwindex, hype_presence . harms, hype_presence . damp
        ]

        
-- interface must select a track or a parameter inside a hype or the hype as a track
data Layout = Tracks Int | Parameters Int deriving Show
data Selection = Out Layout | In Int Layout deriving Show
selset y (Out x) = Out y
selset y (In z x) = In z y
selout (In z x) = Out x
selout x = x
selin y (Out x)  = In y x
selin y (In _ x)  = In y x



regchan = 0 
-- send out the state of the selected track or parameter
midiOut :: TVar (M.Map Int Hype)  -> TVar Selection -> TChan () -> IO ()
midiOut thypes top tp = (do
  SndSeq.withDefault SndSeq.Block $ \h -> do
        Client.setName (h :: SndSeq.T SndSeq.OutputMode) "waves"
        c <- Client.getId h
        Port.withSimple h "out" (Port.caps [Port.capRead, Port.capSubsRead]) Port.typeMidiGeneric $ \p -> forever $ do
                () <- atomically $ readTChan tp
                op <- atomically $ readTVar top
                hypes <- atomically $ readTVar thypes
                let send seqs ls las = case las of
                        Tracks prog ->  case M.lookup prog seqs of
                                        Nothing -> return ()
                                        Just s -> do 
                                                forM_ (zip [0..] ls) $ \(par,ml) -> do
                                                        let ev =  Event.forConnection (Connect.toSubscribers (Addr.Cons c p)) $ 
                                                                        Event.CtrlEv Event.Controller (Event.Ctrl 
                                                                                (Event.Channel $ fromIntegral regchan) 
                                                                                (Event.Parameter $ fromIntegral par) 
                                                                                (Event.Value $ fromIntegral $ s ^. ml)
                                                                                )
                                                        void $ Event.outputDirect h $ ev 
                                                void $ Event.outputDirect h $  Event.forConnection (Connect.toSubscribers (Addr.Cons c p)) $ 
                                                                Event.CtrlEv Event.Controller (Event.Ctrl 
                                                                        (Event.Channel $ fromIntegral regchan) 
                                                                        (Event.Parameter $ fromIntegral 127) 
                                                                        (Event.Value $ fromIntegral $ prog)
                                                                        )

                        Parameters par ->  case par >= 48 of
                                        True -> return ()
                                        False -> do 
                                                forM_ (M.assocs seqs) $ \(ctrl,prog) -> do
                                                        let ev =  Event.forConnection (Connect.toSubscribers (Addr.Cons c p)) $ 
                                                                        Event.CtrlEv Event.Controller (Event.Ctrl 
                                                                                (Event.Channel $ fromIntegral regchan) 
                                                                                (Event.Parameter $ fromIntegral ctrl) 
                                                                                (Event.Value $ fromIntegral $ prog ^. (ls !! par))
                                                                                )
                                                        void $ Event.outputDirect h $ ev 
                                                void $ Event.outputDirect h $  Event.forConnection (Connect.toSubscribers (Addr.Cons c p)) $ 
                                                                Event.CtrlEv Event.Controller (Event.Ctrl 
                                                                        (Event.Channel $ fromIntegral regchan) 
                                                                        (Event.Parameter $ fromIntegral 126) 
                                                                        (Event.Value $ fromIntegral $ par)
                                                                        )
                case op of
                        In x y -> send ((hypes M.! x) ^. hype_set) seq_lenses y
                        Out x -> send hypes hype_lenses x
                print op 
                ) `AlsaExc.catch` \e -> putStrLn $ "alsa_exception: " ++ AlsaExc.show e
ntracks = 24

inside n m v  | v < n = n
              | v > m = m
              | True = v  

-- wait an event , modify the state or the selection and send an update in case
-- midiIn :: TVar (M.Map Int Hype) -> TVar Selection -> TChan () -> TVar (Double,Double,Int)
midiIn thypes tselection tupdate ttmp = (do
  tcopy <- newTVarIO 0
  SndSeq.withDefault SndSeq.Block $ \h -> do
        Client.setName (h :: SndSeq.T SndSeq.InputMode) "Ws"
        c <- Client.getId h
        Port.withSimple h "control in" (Port.caps [Port.capWrite, Port.capSubsWrite]) Port.typeMidiGeneric $ \p -> forever $ do
                ev <-  Event.input h
                case Event.body ev of
                     Event.CtrlEv Event.Controller (Event.Ctrl 
                                        (Event.Channel cha) 
                                        (Event.Parameter par) 
                                        (Event.Value val)
                                        ) -> do
                        let n = fromIntegral cha
                            v = fromIntegral val  
                            v' = v  
                            pa = fromIntegral par
                        zo <- atomically $ readTVar tselection
                        when  (n == regchan) $ do 
                                 print (par,v,zo)
                                 case pa of 
                                        
                                        127 -> atomically $ when (v < ntracks) $ modifyTVar tselection (selset $ Tracks v') >> writeTChan tupdate ()
                                        126 -> atomically $ when (v < 48) $ modifyTVar tselection (selset $ Parameters v') >> writeTChan tupdate ()
                                        125 -> case v of 
                                                127 -> atomically $ modifyTVar tselection selout >> writeTChan tupdate ()
                                                _ -> return ()
                                        124 -> atomically $ when (v < ntracks) $ modifyTVar tselection (selin v) >> writeTChan tupdate ()
                                        123 -> atomically $ modifyTVar ttmp $ \(sp,sh,ph) -> (fromIntegral v,sh,ph)
                                        122 -> atomically $ modifyTVar ttmp $ \(sp,sh,ph) -> (sp,(fromIntegral v / 127) * 15 / sp,ph)
                                        121 -> atomically $ modifyTVar ttmp $ \(sp,sh,ph) -> (sp,sh,v)
                                        120 -> readFile session >>= \(read -> (tm,ts)) -> atomically $ do 
                                                writeTVar thypes ts
                                                writeTVar ttmp tm
                                                writeTChan tupdate ()

                                        119 -> do
                                                ts <- atomically $ readTVar thypes
                                                tm <- atomically $ readTVar ttmp
                                                writeFile session (show (tm,ts))
                                        118 -> when (v < ntracks) $ atomically $ writeTVar tcopy v
                                        117 -> atomically $ case v of 
                                                127 -> do
                                                        op <- readTVar tselection
                                                        copy <- readTVar tcopy
                                                        case op of
                                                                Out (Tracks x) -> modifyTVar thypes $ \ts -> M.insert copy (ts M.! x) ts
                                                                In x (Tracks y) -> modifyTVar thypes $ M.adjust (over hype_set (\ts -> M.insert copy (ts M.! y) ts)) x                                          
                                                                _ -> return ()
                                                _ -> return ()
                                        _ ->   atomically $ do  
                                                        op <- readTVar tselection
                                                        case op of
                                                                In x y -> case y of 
                                                                        Tracks prog -> when (pa < 48) $ modifyTVar thypes $ 
                                                                                M.adjust  (over hype_set (M.adjust ((seq_lenses !! pa) .~ v) prog)) x
                                                                        Parameters ctrl -> when (pa < ntracks) $ modifyTVar thypes $ M.adjust
                                                                                (over hype_set (M.adjust ((seq_lenses !! ctrl) .~ v) pa)) x
                                                                Out y -> case y of
                                                                        Tracks prog -> when (pa < 48) $ modifyTVar thypes $ 
                                                                                M.adjust  ((hype_lenses !! pa) .~ v) prog 
                                                                        Parameters ctrl -> when (pa < ntracks) $ modifyTVar thypes $ M.adjust
                                                                                ((hype_lenses !! ctrl) .~ v) pa
                     _ -> return ()
                        
        )
                `AlsaExc.catch` \e -> putStrLn $ "alsa_exception: " ++ AlsaExc.show e

delay = 0.1
sampledir = "/home/paolino/Music/samples"
session  = "current.sxn"
type Params = M.Map String Double
data Sound = Sound String Double Double Double
data QueryS = BootSample (IO ()) Sound | PlaySample Sound | DoNo

main = do 
        boot
        (lsamples ,samples) <- initSamples  sampledir
        tmp <- newTVarIO $ (125,0,0)
        let tracks = zip [0..] $ map (\_ -> Seq 1 0 (W 64 16 0  0 0 0 0)(W 64 16 0  0 0 0 0) (W 64 16 0  0 0 0 0) (W 64 16 0  0 0 0 0) (W 64 16 0  0 0 0 0)) [0..ntracks - 1]
        thypes <- newTVarIO $ M.fromList $ zip [0..] $ map (\_ -> Hype 8 0 (M.fromList tracks) (W 64 16 0 0 0 0 0)(W 64 16 0  0 0 0 0)) [0..ntracks - 1]
        tupdate <- newTChanIO 
        tprog <- newTVarIO $ In 0 (Tracks 0)
        forkOS $ midiIn  thypes tprog tupdate tmp
        forkOS $ midiOut thypes tprog tupdate
        let z t w samples = do -- duty
                        hs <- atomically $ fmap M.elems $ readTVar thypes
                        (d,e,dw)  <- atomically $ readTVar tmp
                        sleepThreadUntil t -- wait next tick
                        let     parsets = flip map hs $ \(Hype sp psp pase wpres wprec) ->
                                        let     pr = (\(x:y:_) -> (signum (x * y) + 1)/2 ) $ valueW (w' + dw) wpres
                                                pw= (^2).head $ valueW (w' + dw) wprec
                                                (quot,res) = if sp == 0 then (1,0) else divMod (w + psp) sp
                                                w' = quot
                                        in (pase,if pr > 0 && res == 0 then pw else 0)
                                ts = fst . head $ sortBy (comparing snd) parsets
                                (samples',qs) = (\f -> mapAccumR f samples $ M.elems ts) $ \samples (Seq sp psp ws wpr wpw wpi wgr) -> 
                                       let      pr = (\(x:y:_) -> (signum (x * y) + 1)/2 ) $ valueW (w' + dw) wpr
                                                pw= (^2).head $ valueW (w' + dw) wpw
                                                [pi,gr,pso] = map (head . valueW (w' + dw)) [wpi,wgr,ws] 
                                                
                                                (quot,res) = if sp == 0 then (1,0) else divMod (w + psp) sp
                                                w' = quot
                                                test k s = if pr > 0 && res == 0 then k $ Sound s  pw pi gr else DoNo
                                                s =  floor $ (pso + 1) * fromIntegral lsamples
                                       in case s `M.member` samples of
                                                False -> (samples, DoNo)
                                                True -> case  samples M.! s of
                                                                Right z -> (samples, test PlaySample z)
                                                                Left (p,z) -> (M.insert s (Right z) samples, test (BootSample p) z)
                                solve DoNo = return ()
                                solve (PlaySample s) = sound (t  + delay + e) s
                                solve (BootSample p s) = p >> sound (t + delay + e) s

                        mapM_ solve qs -- resolve the harvest and send the bundle
                        z (t + 60/4/d) (w + 1) $ samples' -- next tick
        t <- time -- read actual time
        z t 0 samples

----------------- Directory ------------------------------

initSamples :: FilePath -> IO (Int, M.Map Int (Either (IO (), String) String))
initSamples sampledir = do
        ls <- map (id &&& takeBaseName)  `fmap` (find (depth ==? 0) (extension ==? ".wav") sampledir)
        let bs =  flip zip (map snd ls) . zipWith bootSample [0 .. ] $ ls
        return (length ls, M.fromList $ zip [0..] $ map Left bs)

------------------------------------------------------------------
---------------- Supecollider zone --------------------------------
------------------------------------------------------------------

sound :: Time -> Sound -> IO ()
sound mt (Sound s amp pitch groove) = do
        withSC3 . sendBundle . bundle (mt + groove/10) $ [s_new s (-1) AddToTail 1 $ [("amp",amp),("rate",1 + pitch)]]
        
bootSample :: Int -> (FilePath,String) -> IO ()
bootSample n (fp,i) = do
        withSC3 . send $ b_allocRead n fp 0 0
        withSC3 . send $ d_recv . synthdef i . out 0 $ 
                        control KR "amp" 1 * playBuf 2 AR (fromIntegral n) (control KR "rate" 1) 0 0 NoLoop RemoveSynth  
        print fp

boot :: IO ()
boot =  withSC3 . send $ p_new [(1, AddToTail, 0)] 
{-
        forkIO $ forever $ runInputT  (defaultSettings{ historyFile = Just "waves.hist", autoAddHistory = True}) $ do
                        p <- liftIO $ atomically (readTVar tmp) 
                        ts <- liftIO $ atomically $ readTVar ttracks
                        o <- getInputLineWithInitial ":>" ("","")
                        case o of 
                                Nothing -> return ()
                                Just o -> do 
                                        let (p,ts) = read o
                                        liftIO $ atomically $ writeTVar tmp p
                                        liftIO $ atomically $ writeTVar ttracks (M.fromList $ zip [1..] ts)
-}
