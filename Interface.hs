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
        tselection <- newTVarIO zero :: IO (TVar (PersistentChoice PInt))
        t1 <- forkIO . forever $ report output tinterface tselection tupdate        
        t2 <- forkIO . forever $ aquire input  tinterface tselection tupdate
        forkIO $ sleepThread 3 >> atomically (writeTChan tupdate ())
        return (killThread t1 >> killThread t2, tselection)




-- send out the state of the selected track or parameter
report :: TChan (Int,Int) -> TVar Interface ->  TVar (PersistentChoice PInt) -> TChan () -> IO ()
report midiout  tinterface  top tp = do 
        () <- atomically $ readTChan tp
        -- see what interface is selected
        op <- atomically $ readTVar top
        case op ^. persistentChoice of
                -- patterns are selected
                A prog -> do
                        Interface seqs projs assocs <- atomically $ readTVar tinterface
                        let     Realize (i,j) assoc = assocs IM.! fromIntegral prog
                                proj = projs IM.! i
                        case querySequenza seqs (fromIntegral j) of
                                Nothing -> return ()
                                Just (ps,_,_) -> atomically $ do      
                                        forM_ (zip [0.. 23] sequenza_lenses) $ \(par,ml) -> writeTChan midiout (par, ps ^. core . ml)
                                        forM_ (zip [24..28] projection_lenses) $ \(par,ml) -> writeTChan midiout (par, proj ^. ml)
                                        writeTChan midiout (29,i)
                                        writeTChan midiout (30,j)
                                        case assoc of 
                                                Left (SControl bus) -> writeTChan midiout (31, bus)
                                                Right x -> forM_ (zip [31 ..] synth_lenses) $ \(par,sl) -> writeTChan midiout (par, x ^. sl)

                                        


aquire :: TChan (Int,Int) -> TVar Interface -> TVar (PersistentChoice PInt) -> TChan () -> IO ()
aquire  midiin tinterface  tselection tupdate  = do
        (par,val) <- atomically $ readTChan midiin
        print (par,val)
        Interface seqs projs ssamples <- atomically $ readTVar tinterface
        print projs
        atomically $ do
        
                writeTChan tupdate ()
                case par of 
                        127 ->  modifyTVar tselection $ (choice .~ PInt val)
                        126 ->  modifyTVar tselection nextChoice
                        _ -> do
                                op <- readTVar tselection
                                case op ^. persistentChoice of
                                        A (PInt prog) -> do 
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
                                        _ -> return ()
        Interface seqs projs ssamples <- atomically $ readTVar tinterface
        print projs
        
                        

lookupAndChange xs ys x f = maybe (return ()) f $ lookup x $ zip xs ys 

