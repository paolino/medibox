module Dynamic where 

import Prelude hiding (lookup)
import Data.IntMap hiding (map, foldr)

import Graph (acyclic, pointing, Graph)
import Haskell (mapAccumM)

data Pointing a b = Pointing b | Base a 

type Dynamic a b c = IntMap (Pointing a b , Maybe c)

query :: (b -> [Int]) -> ([c] -> b -> c) -> (a -> c) -> Dynamic a b c -> Int -> Maybe (Pointing a b , c, Dynamic a b c)
query fd fp fb d k = do 
        let positive d p y = (p, y, insert k (p,Just y) d) 
        r <-  k `lookup` d 
        case r of 
                (p@(Base x) ,Nothing) -> return $ positive d p $ fb x
                (p@(Pointing x), Nothing) -> do
                        let sin d k' = do
                                (_,y',d') <- query fd fp fb d k'
                                return (d',y')
                        (d',ys) <- mapAccumM sin d (fd x)
                        return $ positive d' p $ fp ys x
                (p,Just y) -> return $ positive d p y 


touch :: [Int] -> Dynamic a b c -> Dynamic a b c
touch ks d = foldr (adjust (\(x,_) -> (x,Nothing))) d ks 

dyninsert :: (b -> [Int]) -> Int -> Pointing a b -> Dynamic a b c -> Maybe (Dynamic a b c)
dyninsert fd k p d = let
        d' = insert k (p,Nothing) d
        g = dyndeps fd d'
        in if acyclic g then  
                Just $ touch (pointing k g) d'
                else Nothing

dyndeps :: (b -> [Int]) -> Dynamic a b c -> Graph Int
dyndeps fd d = assocs d >>= f where
        f (_,(Base _,_)) = []
        f (k1, (Pointing x, _)) = map ((,) k1) (fd x)
