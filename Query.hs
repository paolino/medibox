module Query where

import Control.Concurrent.STM
import qualified Data.IntMap as IM
import Haskell


data Query m k a = Query (k -> m a) ((k,a -> a) -> m ()) (m (k,a))

intmapQ :: Present a => STM (Query STM Int a)
intmapQ = do
        x <- newTVar zero 
        y <- newTChan
        let set (k,f) = do
                mz <- (flip IM.lookup k) `fmap` readTVar x
                case mz of 
                        Nothing -> return ()
                        Just z -> do
                                let z' = f z
                                writeTChan y (k,z')
                                modifyTVar x $ IM.insert k z'
        return $ Query (\k -> IM.findWithDefault zero k `fmap` readTVar x) set (readTChan y)

