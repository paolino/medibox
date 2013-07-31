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


data FP = FP {
        _ffreq :: Int , _fpower :: Int,_fphase :: Int}
        deriving (Show,Read)

derivative :: FP -> FP 
derivative (FP w r k) = FP w (r * w) 0


$(makeLenses ''FP)

type  Fourier = (FP,FP,FP,FP,FP,FP,FP,FP)

derivativeFourier (w1,w2,w3,w4,w5,w6,w7,w8) = (derivative w1, derivative w2,derivative w3,derivative w4,derivative w5,derivative w6,derivative w7,derivative w8) 

seno (FP w r k) t =  from128 r * sin (2 * pi * from128 k + fromIntegral w * t)

fourier (w1,w2,w3,w4,w5,w6,w7,w8) t = sum . map (flip seno t) $ [w1,w2,w3,w4,w5,w6,w7,w8]

times :: Int -> [Double]
times l = take l $ [0,2*pi/fromIntegral l .. ]




from128 x = 1/128*fromIntegral x

valueW :: Int -> Fourier -> Int -> Int  -> [Double]
valueW w ims  l' d = let   
        l = l'
        qs = map (fourier ims) $ times l
        mqs = maximum $ map abs qs
        qs' = if mqs > 0 then map (/mqs) qs else qs
        in take l . drop (w `mod` l) . cycle $ map (from128 d *) $ qs'



data Seq = Seq 
        { _wave :: Fourier
        , _sample :: Int
        , _pitch :: Int -- pitch
        , _groove :: Int -- groove
        , _width :: Int
        , _damp :: Int
        , _dilatation :: Int
        , _shift :: Int
        }
        deriving (Show,Read)

        



$(makeLenses ''Seq)

nolens  = lens (\_ -> 0) (\s _ -> s)

seq_lenses = 
        [  wave . _1 . ffreq,wave . _2 . ffreq,wave . _3 . ffreq,wave . _4 . ffreq,wave . _5 . ffreq,wave . _6 . ffreq,wave . _7 . ffreq,wave . _8 . ffreq
        ,  wave . _1 . fpower,wave . _2 . fpower,wave . _3 . fpower,wave . _4 . fpower,wave . _5 . fpower,wave . _6 . fpower,wave . _7 . fpower,wave . _8 . fpower
        ,  wave . _1 . fphase,wave . _2 . fphase,wave . _3 . fphase,wave . _4 . fphase,wave . _5 . fphase,wave . _6 . fphase,wave . _7 . fphase,wave . _8 . fphase
        , sample, pitch, groove,  width,  shift,  dilatation, damp]


        
-- interface must select a track or a parameter inside a hype or the hype as a track
data Selection = Tracks Int | Parameters Int deriving Show



regchan = 0 
-- send out the state of the selected track or parameter
midiOut :: TVar (M.Map Int Seq)  -> TVar Selection -> TChan () -> IO ()
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

                        Parameters par ->  case par >= 31 of
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
                send hypes seq_lenses op
                print op 
                ) `AlsaExc.catch` \e -> putStrLn $ "alsa_exception: " ++ AlsaExc.show e
ntracks = 24
session  = "current.sxn"

inside n m v  | v < n = n
              | v > m = m
              | True = v  

-- wait an event , modify the state or the selection and send an update in case
-- midiIn :: TVar (M.Map Int Seq) -> TVar Selection -> TChan () -> TVar (Double,Double,Int)


midiIn  thypes tselection tupdate ttmp = (do
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
                                        127 -> atomically $ when (v < ntracks) $ writeTVar tselection (Tracks v) >> writeTChan tupdate ()
                                        126 -> atomically $ when (v < 30) $ writeTVar tselection (Parameters v) >> writeTChan tupdate ()
                                        125 -> atomically $ modifyTVar ttmp $ \(n,sp,sh,ph) -> (n,n * fromIntegral v,sh,ph)
                                        124 -> atomically $ modifyTVar ttmp $ \(n,sp,sh,ph) -> (n,sp,(fromIntegral v / 128) * 15 / sp / n,ph)
                                        123 -> atomically $ modifyTVar ttmp $ \(n,sp,sh,ph) -> (n,sp,sh,v)
                                        118 -> atomically $ modifyTVar ttmp $ \(n,sp,sh,ph) -> (fromIntegral v,sp,sh,ph)
                                        122 -> readFile session >>= \(read -> (tm,ts)) -> atomically $ do 
                                                writeTVar thypes ts
                                                writeTVar ttmp tm
                                                writeTChan tupdate ()

                                        121 -> do
                                                ts <- atomically $ readTVar thypes
                                                tm <- atomically $ readTVar ttmp
                                                writeFile session (show (tm,ts))
                                        120 -> atomically $ case v of 
                                                        127 -> do 
                                                                op <- readTVar tselection 
                                                                case op of (Tracks l) -> writeTVar tcopy l
                                                                           _ -> return ()
                                                        _ -> return ()
                                        119 -> atomically $ case v of 
                                                        127 -> do 
                                                                op <- readTVar tselection 
                                                                case op of (Tracks l) -> do
                                                                                        v <- readTVar tcopy 
                                                                                        modifyTVar thypes $ \s -> (M.insert l (s M.! v) s)
                                                                                        writeTChan tupdate ()
                        
                                                                           _ -> return ()
                                                        _ -> return ()
                                        _ ->   atomically $ do  
                                                        op <- readTVar tselection
                                                        case op of
                                                                Tracks prog -> when (pa < 31) $   modifyTVar thypes $ M.adjust ((seq_lenses !! pa) .~ v) prog 
                                                                Parameters ctrl -> when (pa < ntracks) $ modifyTVar thypes $ 
                                                                        M.adjust ((seq_lenses !! ctrl) .~ v) pa
                     _ -> return ()
                        
        ) `AlsaExc.catch` \e -> putStrLn $ "alsa_exception: " ++ AlsaExc.show e

delay = 0.1
sampledir = "/home/paolino/Music/samples"
type Params = M.Map String Double
data Sound = Sound String Double Double Double
data QueryS = BootSample (IO ()) Sound | PlaySample Sound | DoNo

main = do 
        trr <- boot
        (lsamples ,samples) <- initSamples  sampledir
        tmp <- newTVarIO $ (1,125,0,0)
        let tracks = zip [0..] $ map (\_ -> Seq (FP 1 0 0, FP 2 0 0,FP 3 0 0,FP 4 0 0,FP 5 0 0,FP 6 0 0,FP 7 0 0,FP 8 0 0) 0  64 64 2 0 1 0) [0..ntracks - 1]
        thypes <- newTVarIO $ M.fromList tracks 
        tupdate <- newTChanIO 
        tprog <- newTVarIO $ (Tracks 0)
        tplay <- newTVarIO 0
        forkOS $ midiIn thypes tprog tupdate tmp
        forkOS $ midiOut thypes tprog tupdate
        let z t w samples = do -- duty
                        sleepThreadUntil t -- wait next tick
                        (n,d,e,dw)  <- atomically $ readTVar tmp
                        ts <- atomically $ readTVar thypes
                        let     (samples',qs) = (\f -> mapAccumR f samples $ M.elems ts) $ \samples (Seq fou sa pi gr width da dila shift) -> 
                                       let      wv = valueW (quo + shift) fou width da
                                                wpr = valueW (quo + shift) (derivativeFourier fou) width 1
                                                pw = case  wv of 
                                                        [] -> 0
                                                        (x:_) -> abs x
                                                pr = case  wpr of 
                                                        [] -> False
                                                        [_] -> False
                                                        (x:y:_) -> signum (x * y) <= 0 && y /= 0
                                                test k s =  if pr && pw > 0 && r == 0 then k $ Sound s  pw (from128 pi - 0.5) (from128 gr - 0.5) else DoNo
                                                s = sa `mod` lsamples
                                                (quo,r) = (w + dw)  `divMod` dila
                                       in case  samples M.! s of
                                                                Right z -> (samples, test PlaySample z)
                                                                Left (p,z) -> (M.insert s (Right z) samples, test (BootSample p) z)
                                solve DoNo = return ()
                                solve (PlaySample s) = sound trr (t  + delay + e) s
                                solve (BootSample p s) = p  >> sound trr (t + delay + e) s
                        mapM_ solve qs -- resolve the harvest and send the bundle
                        z (t + 60/4/d/n) (w + 1) samples' -- next tick
        t <- time -- read actual time
        z t 0 samples 



----------------- Directory ------------------------------

servers = [57110,57111 .. 57117]

initSamples :: FilePath -> IO (Int, M.Map Int (Either (IO (), String) String))
initSamples sampledir = do
        ls <- map (id &&& takeBaseName)  `fmap` (find (depth ==? 0) (extension ==? ".wav") sampledir)
        print ls
        sequence_ $ do 
                s <- servers
                l <- zip [0..] ls
                return $ uncurry (bootSample s) l

        return (length ls, M.fromList $ zip [0..] $ map (Right . snd) ls)

------------------------------------------------------------------
---------------- Supecollider zone --------------------------------
------------------------------------------------------------------

sound :: TVar [Int] -> Time -> Sound -> IO ()
sound trr mt (Sound s amp pitch groove) = do
        i <- atomically $ do
                (i:is) <- readTVar trr
                writeTVar trr is
                return i
        print (i,s)
        withSC3n i . sendBundle . bundle (mt + groove/10) $ [s_new s (-1) AddToTail 1 $ [("amp",amp),("rate",1 + pitch)]]
        
bootSample :: Int -> Int -> (FilePath,String) -> IO ()
bootSample j n (fp,i) = do
        withSC3n j . send $ b_allocRead n fp 0 0
        withSC3n j . send $ d_recv . synthdef i . out 0 $ 
                        control KR "amp" 1 * playBuf 2 AR (fromIntegral n) (control KR "rate" 1) 0 0 NoLoop RemoveSynth  

boot :: IO (TVar [Int])
boot =  do 
        forM_ servers $ \i -> withSC3n i . send $ p_new [(1, AddToTail, 0)]
        newTVarIO $ cycle servers
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
withSC3n :: Int -> Connection UDP a -> IO a
withSC3n i = withTransport (openUDP "127.0.0.1" $ fromIntegral i)
