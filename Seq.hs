
{-# LANGUAGE ViewPatterns, ScopedTypeVariables, TemplateHaskell #-}
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Control.Exception
import Sound.OSC
import Project
import MidiComm
import qualified Data.Map as M
import Control.Lens
import Control.Lens.TH
import Data.List
import Data.Ord
import System.Random
import Control.Arrow
import Data.Traversable


type Controls = M.Map Int Int
readValue :: Int -> Double -> Double -> Controls -> Double
readValue i f s = (+s) . (*f) . (/128) . fromIntegral . M.findWithDefault 0 i

data Morph = Morph {
  _shiftNote :: Int 
  , _zoomVolume ::Double
  }deriving (Show,Read)

makeLenses ''Morph


type TrackId = Int

readMorph :: TrackId -> Controls -> Morph
readMorph n m = Morph 
  (floor $ readValue n 50 0 m) 
  (readValue (24 + n) 1 0 m)
        

morph :: Morph -> Er -> Er
morph (Morph dn vo) = over (event . _Nota) $ \(n,v,d) -> 
    (n + dn, floor (vo * fromIntegral v),d)

data Displace = Displace {
  _shiftTime :: Time , 
  _expandTime :: Time 
  }deriving (Show,Read)

makeLenses ''Displace

displace :: Displace -> Er -> Er
displace (Displace dt ro) = over timet ((+dt) . (*ro))

readDisplace n m =  Displace 
  (readValue (16 + n) 4 0 m) 
  (readValue (8 + n)  2 0.5 m)
  
data Track = Track {
  _morpher :: Morph,
  _displacer ::  Displace,
  _eventi :: [Er],
  _aquiring ::Bool
  } deriving (Show,Read)

makeLenses ''Track 

dispatch :: TChan Er -> Time -> Track -> STM ()
dispatch tc t (Track m d es a) = mapM_ (writeTChan tc . displace (over shiftTime (+t) d) . morph m) es 
    

updateTrack :: TrackId -> Controls -> Track -> Track
updateTrack i m (Track _ _ es a) = Track (readMorph i m ) (readDisplace i m)  es a


updateControls :: TVar Int -> TChan Er -> TVar Controls -> IO ThreadId
updateControls selected inp ti = forkIO . forever . atomically $ do
                e <- readTChan inp
                case e of
                    E (Controllo p v) _ -> do 
                        modifyTVar ti $ M.insert p v
                        writeTVar selected $ p `mod` 8
                    _ -> return ()
               
type Tracks = M.Map Int Track 



-- listen to notes and substitute the nearest event
updateNotes :: TVar Int -> TVar Time -> TChan Er -> TVar Tracks -> IO ThreadId
updateNotes selected zt inp tt = forkIO . forever . atomically $ do 
                    E e te' <- readTChan inp
                    te <- flip subtract te' <$> readTVar zt
                    s <- readTVar selected
                    modifyTVar tt $ flip  M.adjust s $ \tr -> 
                              let   g :: [Er] -> [Er]
                                    g [] = []
                                    g es = let 
                                        E _ t:xs = sortBy (comparing $ abs . subtract te . view timet) es
                                        in E e t:xs
                                        
                              in over eventi g tr


data L = LR R | P | T deriving Read 
data R = R Int Int Int Int Int Int Time Time deriving Read 

randomNotes :: R -> IO [Er]
randomNotes (R s n p0 dp v0 dv dt dl) = do
    setStdGen (mkStdGen s)
    forM [1 .. n] $ \_ -> do
      p <- randomRIO (p0,p0 + dp)
      v <- randomRIO (v0,v0 + dv)
      t <- randomRIO (0,dt)
      dl <- randomRIO (0,dl)
      return $ E (Nota p v dl) t 

l = LR (R 0 5 48 0 30 80 2 0.5)


input selected tracks controls exit = forkIO .forever $ do 
      l <- getLine
      join . atomically $ case reads l of 
                      [(P,_)] -> return $ readTVarIO controls >>= print
                      [(T,_)] -> return $ readTVarIO tracks >>= mapM_ print
                      [(LR r,_)] -> do
                        return $ do 
                                  es <- randomNotes $ r
                                  atomically $ do
                                          t <- readTVar selected
                                          modifyTVar tracks $ M.adjust (set eventi es) t
                                  
                      _ ->   writeTVar exit True >> return (return ())

main = do
  incn <- midiIn "notes" 0 
  out <- (!! 0) <$> midiOut "test" 0 1 
  atomically $ writeTVar (active out) True
  incc <- midiIn "controls" 1 

  term <- newTChanIO
  
  controls <- newTVarIO M.empty
  selected <- newTVarIO 0
  updateControls selected incc controls

  tracks <- newTVarIO $ M.fromList $ zip [0..7] $ repeat $ Track (Morph 0 100) (Displace 0 1) [E (Nota 40 40 0.5) 0.3] False
  
  t0 <- time
  zerotime <- newTVarIO t0
  exit <- newTVarIO False
  input selected tracks controls exit
  -- updateNotes selected zerotime incn tracks
  let loop n = do
        let t = t0 + 2*n
        
        atomically $ do
            writeTVar zerotime t
            m <- readTVar controls
            ts <- readTVar tracks
            let   ts' = M.mapWithKey (\n -> updateTrack n m) ts
            forM_ ts' (dispatch (events out) t) 
            writeTVar tracks ts'
        sleepThreadUntil $ t
        c <- readTVarIO exit
        if c then return () else loop $ n + 1
  loop 1
