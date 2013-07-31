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
        
events :: Pat -> [Tempo]
events (Pat n w s) = [from128 s + 1 / fromIntegral w / fromIntegral n * fromIntegral j | j <- [0 .. n - 1]]

data FP = FP 
        { _ffreq :: Int 
        , _fpower :: Int
        , _fphase :: Int
        }
        deriving (Show,Read)

$(makeLenses ''FP)

type  Fourier = M.Map Int FP
type  Presence = M.Map Int Pat

seno :: FP -> Tempo -> Double
seno (FP w r k) t =  from128 r * sin (2 * pi * from128 k + fromIntegral w * t)


fourier :: Fourier -> Tempo -> Double
fourier fou t = sum . map (flip seno t) $ M.elems fou

times :: Int -> [Tempo]
times l = take l $ [0,2*pi/fromIntegral l .. ]


schedule :: Presence -> [Tempo]
schedule pre = sort . concat $ every (M.elems pre) events



wave :: [Tempo] -> Fourier -> [Double]
wave ts ims = let   
        qs = map (fourier ims) $ ts
        mqs = maximum $ map abs qs
        in if mqs > 0 then map (/mqs) qs else qs


data Seq = Seq 
        { _ampl :: Fourier
        , _pres :: Presence
        , _sample :: Int
        , _pitch :: Int -- pitch
        , _groove :: Int -- groove
        --, _width :: Int
        , _damp :: Int
        , _dilatation :: Int
        , _shift :: Int
        }
        deriving (Show,Read)

$(makeLenses ''Seq)

noseq = Seq 
        (M.fromList $ zip [0..3] $ repeat (FP 1 0 0)) 
        (M.fromList $ zip [0..3] $ repeat (Pat 0 0 0))
        0
        64
        64
        64
        1
        0


sequenza ::  Seq -> [(Tempo,Double)]
sequenza  (Seq a p s pi gr da di sh) = let 
        ts = schedule p
        as = every (wave ts a) (from128 da *)
        ts' = every ts $ \t ->  from128 sh + (fromIntegral di) *t + from128p gr
        in zip ts' as

nolens  = lens (\_ -> 0) (\s _ -> s)

male n = lens (\m -> m M.! n) (flip $ M.insert n)

seq_lenses = 
        [  pres . male 0 . pnumber
        ,  pres . male 1 . pnumber
        ,  pres . male 2 . pnumber
        ,  pres . male 3 . pnumber
        ,  ampl . male 0 . ffreq
        ,  ampl . male 1 . ffreq
        ,  ampl . male 2 . ffreq
        ,  ampl . male 3 . ffreq
        ,  pres . male 0 . pwidth
        ,  pres . male 1 . pwidth
        ,  pres . male 2 . pwidth
        ,  pres . male 3 . pwidth
        ,  ampl . male 0 . fpower
        ,  ampl . male 1 . fpower
        ,  ampl . male 2 . fpower
        ,  ampl . male 3 . fpower
        ,  pres . male 0 . pshift
        ,  pres . male 1 . pshift
        ,  pres . male 2 . pshift
        ,  pres . male 3 . pshift
        ,  ampl . male 0 . fphase
        ,  ampl . male 1 . fphase
        ,  ampl . male 2 . fphase
        ,  ampl . male 3 . fphase
        , sample, pitch, groove,  shift,  dilatation, damp
        ]

        
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


midiIn  thypes tselection tupdate tsupdate ttmp = (do
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
                                                                Tracks prog -> when (pa < 31) $ do
                                                                        modifyTVar thypes $ M.adjust ((seq_lenses !! pa) .~ v) prog 
                                                                        writeTChan tsupdate prog
                                                                Parameters ctrl -> when (pa < ntracks) $ do
                                                                        modifyTVar thypes $ M.adjust ((seq_lenses !! ctrl) .~ v) pa
                                                                        writeTChan tsupdate pa
                     _ -> return ()
                        
        ) `AlsaExc.catch` \e -> putStrLn $ "alsa_exception: " ++ AlsaExc.show e

delay = 0.1
sampledir = "/home/paolino/Music/samples"
type Params = M.Map String Double

     
data Event = Event 
        { etrack :: Int
        , etime :: Tempo
        , esound :: (Int, Double, Int)
        }

main = do 
        play <- initSamples  sampledir
        tmp <- newTVarIO $ (1,125,0,0)
        let tracks = zip [0..ntracks - 1] $ repeat noseq 
        thypes <- newTVarIO $ M.fromList tracks 
        tupdate <- newTChanIO 
        tsupdate <- newTChanIO 
        tprog <- newTVarIO $ (Tracks 0)
        tevents <- newTVarIO []
        tstartcycle <- newTVarIO 0
        forkOS $ midiIn thypes tprog tupdate tsupdate tmp
        forkOS $ midiOut thypes tprog tupdate
        let     fw = 0.1 -- fire window
                w = 4 -- pattern cycle
        let  updateTEvents = forever . atomically $ do 
                        -- wait for interface change a track parameter 
                        n <- fmap (`mod` ntracks) (readTChan tsupdate)
                        s@(Seq fou pres sa pi gr da dila shift) <- fmap (M.! n) (readTVar thypes)
                        modifyTVar tevents $ \ts -> let 
                                ts' = filter  ((/=n) . etrack) ts
                                nes = map (\(t,a) -> Event n t (sa,a,pi)) (sequenza s)
                                in sortBy (comparing etime) $ nes ++ ts'
             fireEvents t = do    
                        sleepThreadUntil t -- wait next tick
                        (t0,ps) <- atomically $ do
                                t0 <- readTVar tstartcycle
                                es <- fmap (dropWhile ((< t) . (+t0) . (*w) . etime)) $ readTVar tevents
                                return $  (t0,takeWhile ((< t) . (+ fw) .(+t0) . (*w) . etime ) es)
                        forM_ ps $ \(Event _ t (sa,a,pi)) -> play sa (t0 + w*t + delay) a (from128p pi)
                        fireEvents (t + fw) 
             updateCycle t n = do
                        sleepThreadUntil (t + n * w)
                        atomically $ writeTVar tstartcycle t
                        updateCycle t (n + 1)
        
        now <- time 
        forkIO $ updateCycle now 0
        forkIO $ fireEvents now
        updateTEvents
        



------------------------------------------------------------------
---------------- Supecollider zone --------------------------------
------------------------------------------------------------------

servers = [57110,57111 .. 57117]

initSamples :: FilePath -> IO (Int -> Tempo -> Double -> Double -> IO ())
initSamples sampledir = do
        forM_ servers $ \i -> withSC3n i . send $ p_new [(1, AddToTail, 0)]
        ls <- map (id &&& takeBaseName)  `fmap` (find (depth ==? 0) (extension ==? ".wav") sampledir)
        print ls
        sequence_ $ do 
                s <- servers
                l <- zip [0..] ls
                return $ uncurry (bootSample s) l
        let     msamples = M.fromList $ zip [0..] $ map snd ls
        trr <- newTVarIO $ cycle servers
        return (\i -> sound trr (msamples M.! (i `mod` length ls))) 


sound :: TVar [Int] -> String -> Tempo -> Double -> Double -> IO ()
sound trr s mt amp pitch = do
        i <- atomically $ do
                (i:is) <- readTVar trr
                writeTVar trr is
                return i
        print (i,s,mt,amp,pitch)
        withSC3n i . sendBundle . bundle mt $ [s_new s (-1) AddToTail 1 $ [("amp",amp),("rate",1 + pitch)]]
        
bootSample :: Int -> Int -> (FilePath,String) -> IO ()
bootSample j n (fp,i) = do
        withSC3n j . send $ b_allocRead n fp 0 0
        withSC3n j . send $ d_recv . synthdef i . out 0 $ 
                        control KR "amp" 1 * playBuf 2 AR (fromIntegral n) (control KR "rate" 1) 0 0 NoLoop RemoveSynth  

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
