{-# LANGUAGE ViewPatterns, TemplateHaskell #-}
module Interface where

import Control.Concurrent.STM
import Control.Concurrent

import Data.List 

import qualified Data.IntMap as IM
import Control.Arrow
import Control.Monad

import Sound.OSC
import System.IO 
import Control.Lens

import Dynamic (query, Pointing (..), core)
import Sequenza 
import PersistentChoiceLens
import Projections
import Instr
import Realize

type Mem a = IM.IntMap a

data Interface = Interface 
        { _isequenze :: DSequenze
        , _iprojections :: Mem Projection
        , _iassocs :: Mem (Realize (Either SControl Synth)) 
        } deriving (Show,Read)

$(makeLenses ''Interface)

session  = "current.sxn"

-- wait an event , modify the state or the selection and send an update in case
-- midiIn :: TVar (M.Map Int Sequenza) -> TVar Selection -> TChan () -> TVar (Double,Double,Int)
interface input output tinterface  = do
        tupdate <- newTChanIO 
        tselection <- newTVarIO (IM.fromList [(0,0),(1,0),(2,0)]) 
        t1 <- forkIO . forever $ report output tinterface tselection tupdate        
        t2 <- forkIO . forever $ aquire input  tinterface tselection tupdate
        forkIO $ sleepThread 3 >> atomically (writeTChan tupdate ())
        return (killThread t1 >> killThread t2, tselection)




-- send out the state of the selected track or parameter
report :: TChan (Int,Int) -> TVar Interface ->  TVar (IM.IntMap Int) -> TChan () -> IO ()
report midiout  tinterface  top tp = do 
        () <- atomically $ readTChan tp
        -- see what interface is selected
        mpg <- atomically $ readTVar top
                -- patterns are selected
        forM_ (IM.assocs mpg) $ \(w,prog) -> do 
                let wind x = w * 41 + x 
                Interface seqs projs assocs <- atomically $ readTVar tinterface
                let     Realize (i,j) assoc = assocs IM.! fromIntegral prog
                        proj = projs IM.! i
                case querySequenza seqs (fromIntegral j) of
                        Nothing -> return ()
                        Just (ps,_,_) -> atomically $ do      
                                forM_ (zip [wind 0.. wind 23] sequenza_lenses) $ \(par,ml) -> writeTChan midiout (par, ps ^. core . ml)
                                forM_ (zip [wind 24.. wind 28] projection_lenses) $ \(par,ml) -> writeTChan midiout (par, proj ^. ml)
                                writeTChan midiout (wind 29,i)
                                writeTChan midiout (wind 30,j)
                                case assoc of 
                                        Left (SControl bus) -> writeTChan midiout (wind 31, bus)
                                        Right x -> forM_ (zip [wind 31 ..] synth_lenses) $ \(par,sl) -> writeTChan midiout (par, x ^. sl)

                                        


aquire :: TChan (Int,Int) -> TVar Interface -> TVar (IM.IntMap Int) -> TChan () -> IO ()
aquire  midiin tinterface  tselection tupdate  = do
        (par',val) <- atomically $ readTChan midiin
        let (win, par) = par' `divMod` 41
        Interface seqs projs ssamples <- atomically $ readTVar tinterface
        atomically $ do
        
                writeTChan tupdate ()
                case par of 
                        40  ->  modifyTVar tselection $ IM.insert win val
                        _ -> do
                                mpg <- readTVar tselection
                                let prog = mpg IM.! win
                                Interface seqs projs assocs <- readTVar tinterface
                                let     Realize (i,j) assoc = assocs IM.! fromIntegral prog
                                        proj = projs IM.! i
                                when (par >= 0 && par <  24) $ case querySequenza seqs (fromIntegral j) of
                                        Nothing -> return ()
                                        Just (ps,_,_) -> let 
                                                        mseqs = insertSequenza j
                                                                ((core . (sequenza_lenses !! par) .~  val) ps)
                                                                seqs
                                                        in case mseqs of   
                                                                Nothing -> return ()
                                                                Just seqs -> modifyTVar tinterface (isequenze .~ seqs)

                                when (par >= 24 && par < 29) $ 
                                        modifyTVar tinterface (iprojections %~ IM.adjust ((projection_lenses !! (par - 24)) .~ val) i)
                                case lookup par $ zip [29..] [track . _1, track . _2] of
                                        Nothing -> return ()
                                        Just sl -> modifyTVar tinterface (iassocs %~ IM.adjust (sl .~ val) prog)

                                case assoc of
                                        Left (SControl _) -> when (par == 31) $
                                                modifyTVar tinterface (iassocs %~ IM.adjust (what . _Left . coutbus .~ val) prog)
                                        Right x -> case lookup par $ zip [31..] synth_lenses of 
                                                Nothing -> return ()
                                                Just sl -> modifyTVar tinterface (iassocs %~ IM.adjust (what . _Right . sl .~ val) prog)
        
                        

lookupAndChange xs ys x f = maybe (return ()) f $ lookup x $ zip xs ys 

