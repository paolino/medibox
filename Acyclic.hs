{-# LANGUAGE ViewPatterns #-}

-- | Simple graph, acyclic test, roots, reacheable subset of nodes.

module Acyclic where

import Data.List 

-- | A graph as a set of directed links
type Acyclic a = [(a,a)]

-- | Select the subset of nodes which are pointing to a given node, given an equality test for nodes
pointingBy :: (a -> a -> Bool) -> a -> Acyclic a -> [a]
pointingBy f x ys = nubBy f $ pointingBy' x ys where
        pointingBy'  x [] = []
        pointingBy'  x ys = let 
                (map fst -> zs,ts) = partition (f x . snd) ys
                in concatMap (flip pointingBy' ts) zs ++ zs

-- | Specialized pontingBy for Eq instance node

pointing :: Eq a => a -> Acyclic a -> [a]
pointing = pointingBy (==)

-- | Select the subset of points which have no dependencies, given an equality test for nodes

rootsBy :: (a -> a -> Bool) -> Acyclic a -> [a]
rootsBy f zs = let 
        (xs,ys) = unzip zs
        in deleteFirstsBy f (nubBy f ys) xs

-- | Specialized rootBy for Eq instance node

roots :: Eq a =>  Acyclic a -> [a]
roots = rootsBy (==)
       
-- | Determine the absolute acyclicity of a graph, given an equality test for nodes

acyclicBy :: (a -> a -> Bool) -> Acyclic a -> Bool
acyclicBy f xs = let 
        zs = rootsBy f xs
        in acyclicFromBy f zs xs

-- | Specialized acyclicBy for Eq instance node
acyclic :: Eq a => Acyclic a -> Bool
acyclic = acyclicBy (==)

-- | Determine the relative to a given set of nodes of a graph, given an equality test for nodes
acyclicFromBy :: (a -> a -> Bool) -> [a] -> Acyclic a -> Bool
acyclicFromBy f xs ys = let 
        (ms,us) = partition ((\x -> any (f x) xs) . snd)  ys
        in case us of 
                [] -> True
                us -> case intersectBy f (map fst us) xs of 
                        [] ->  case ms of 
                                [] -> False 
                                ms -> acyclicFromBy f (map fst ms ++ xs) us
                        _ -> False

acyclicPapa :: Eq a => [a] -> [(a,a)] -> Bool
acyclicPapa xs ys = case partition ((`elem` xs) . snd) ys of
		(_,[]) -> True
		([],_) -> False
		(us,ms) -> acyclicPapa (map fst ms) us


derootsBy ::  Eq a => (a -> a -> Bool) -> Graph a -> Maybe ([a], Graph a)
derootsBy f [] = Nothing
derootsBy f zs = Just (rs, deleteE rs zs ) where rs = rootsBy f zs

deleteE :: Eq a => [a] -> Graph a -> Graph a
deleteE rs = filter (\(x,y) -> not $ y `elem` rs)

setOf :: Graph a -> [a]
setOf g = map fst g ++ map snd g


resolveBy  :: Eq a => (a -> a -> Bool) -> Graph a -> [a]
resolveBy f g = nub $ (concat $ unfoldr (derootsBy f) g)  ++  setOf g


dfs x [] = [x]
dfs x ys = case partition ((== x) . snd) ys of
	(zs,ys') -> concatMap (flip dfs ys' . fst) zs
