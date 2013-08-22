{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Track (Track (..) , tseq, tproj, DBTrack, DBLens, newDBTrack, scoreOfTrack, dbseq, dbproj, dbtrack) where


import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Control.Monad
import Control.Arrow ((&&&))

import Control.Lens
import Data.Binary
import Score (Sequenza, Tempo, score, Score, Projection, project)

data Track = Track
        {       _tseq :: Int
        ,       _tproj :: Int
        }

$(makeLenses ''Track)

instance Binary Track where
  put (Track a b) = put a >> put b
  get = get >>= \a -> get >>= \b -> return (Track a b)

data DBTrack = DBTrack 
        {       _dbseq' :: IntMap (Sequenza, [Tempo])
        ,       _dbproj' :: IntMap Projection
        ,       _dbtrack' :: IntMap (Track,Maybe (Score Double))
        }

$(makeLenses ''DBTrack)

instance Binary DBTrack where
  put (DBTrack a b c) = put a >> put b >> put c
  get = get >>= \a -> get >>= \b -> get >>= \c -> return (DBTrack a b c)

newDBTrack :: DBTrack
newDBTrack = DBTrack IM.empty IM.empty IM.empty

evalTrack :: DBTrack -> Track -> Maybe (Score Double)
evalTrack db (Track i j)  = do
        p <- db  ^. dbproj' . at j 
        s <- db ^. dbseq' . at i
        return $ project (snd s) p
   

scoreOfTrack :: DBTrack -> Int -> Maybe (Score Double)
scoreOfTrack db n = join . fmap snd $ db ^. dbtrack' . at n

insertSequenza :: Int -> DBTrack -> Maybe Sequenza -> DBTrack
insertSequenza k d s = let
        d' = dbseq' . at k .~ fmap (id &&& score) s $ d
        touch (t, sc) 
                | t ^. tseq == k = (t, evalTrack d' t)
                | otherwise = (t, sc)
        in dbtrack' %~ fmap touch $ d'

insertProjection :: Int -> DBTrack -> Maybe Projection -> DBTrack
insertProjection k d s = let
        d' = dbproj' . at k .~ s  $ d
        touch (t, sc) 
                | t ^. tproj == k = (t, evalTrack d' t)
                | otherwise = (t, sc)
        in dbtrack' %~ fmap touch $ d'

             
insertTrack :: Int -> DBTrack -> Maybe Track -> DBTrack
insertTrack k d s = dbtrack' . at k .~ fmap (id &&& evalTrack d) s $ d

type DBLens a = Functor f => Int -> (Maybe a -> f (Maybe a)) -> DBTrack -> f DBTrack 

dbseq :: DBLens Sequenza 
dbseq j = lens (\d -> fmap fst $ d ^. dbseq'. at j) (insertSequenza j)

dbproj :: DBLens Projection 
dbproj j = lens (\d -> d ^. dbproj'. at j) (insertProjection j)

dbtrack :: DBLens Track 
dbtrack j = lens (\d -> fmap fst $ d ^. dbtrack' . at j) (insertTrack j)
