{-# LANGUAGE ViewPatterns #-}

-- | Simple graph, acyclic test, roots, reacheable subset of nodes.

module Graph where

import Data.List 

-- | A graph as a set of directed links
type Graph a = [(a,a)]

-- | Select the subset of nodes which are pointing to a given node, given an equality test for nodes
pointingBy :: (a -> a -> Bool) -> a -> Graph a -> [a]
pointingBy f x ys = nubBy f $ pointingBy' x ys where
        pointingBy'  x [] = []
        pointingBy'  x ys = let 
                (map fst -> zs,ts) = partition (f x . snd) ys
                in zs ++ concatMap (flip pointingBy' ts) zs

-- | Specialized pontingBy for Eq instance node

pointing :: Eq a => a -> Graph a -> [a]
pointing = pointingBy (==)

-- | Select the subset of points which have no dependencies, given an equality test for nodes

rootsBy :: (a -> a -> Bool) -> Graph a -> [a]
rootsBy f zs = let 
        (xs,ys) = unzip zs
        in deleteFirstsBy f (nubBy f ys) xs

-- | Specialized rootBy for Eq instance node

roots :: Eq a =>  Graph a -> [a]
roots = rootsBy (==)
       
-- | Determine the absolute acyclicity of a graph, given an equality test for nodes

acyclicBy :: (a -> a -> Bool) -> Graph a -> Bool
acyclicBy f xs = let 
        zs = rootsBy f xs
        in acyclicFromBy f zs xs

-- | Specialized acyclicBy for Eq instance node
acyclic :: Eq a => Graph a -> Bool
acyclic = acyclicBy (==)

-- | Determine the relative to a given set of nodes of a graph, given an equality test for nodes
acyclicFromBy :: (a -> a -> Bool) -> [a] -> Graph a -> Bool
acyclicFromBy f xs ys = let 
        (ms,us) = partition ((\x -> any (f x) xs) . snd)  ys
        in case us of 
                [] -> True
                us -> case intersectBy f (map fst us) xs of 
                        [] ->  case ms of 
                                [] -> False 
                                ms -> acyclicFromBy f (map fst ms ++ xs) us
                        _ -> False

