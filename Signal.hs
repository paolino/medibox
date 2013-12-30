{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, KindSignatures, DataKinds, TypeFamilies, PolyKinds, FlexibleContexts, IncoherentInstances, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

import Data.Nat
import Data.Monoid
import Data.Maybe
-- import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Traversable (mapAccumR)
import Control.Arrow (second)

type family W  (s :: Nat -> *) (n :: Nat) :: *

class Signal (s :: Nat -> *) (n :: Nat) where
	next ::  W s (Succ n) -> (M.Map (Idx n) (s n)) -> s (Succ n)

class Source (s :: Nat -> *) where
	source ::  W s Zero -> s Zero


----------------------------------
data Idx (n :: Nat) where
	I :: Integer -> Idx n
	J ::  Idx n -> Idx (Succ n)

type MkIdx n = Integer -> Idx n

index :: Idx n -> (Integer, MkIdx n)
index (I n) = (n,I)
index (J x) = second (J .) $ index x


newKey :: M.Map (Idx n) b -> Integer
newKey g 
	| M.null g = 0
	| otherwise = succ . fst . index . fst $ M.findMax $ g


instance Eq (Idx n) where
	x == y = fst (index x) == fst (index y)

instance Ord (Idx n) where
	x `compare` y = fst (index x) `compare` fst (index y)

data Node s n where
	Dep :: Signal s n => W s (Succ n) -> s (Succ n) -> S.Set (Idx n) -> Node s (Succ n)
	Indep :: W s Zero -> s Zero -> Node s Zero

signal :: Node s n -> s n
signal (Dep w s is) = s
signal (Indep w s) = s

data  Map  (s :: Nat -> *) (n :: Nat) = Map 
	(M.Map (Idx n) (Node s n))
	(Maybe (Map s (Succ n)))

mapAccumRWithKey
  :: (k -> b -> d -> (d, b)) -> d -> M.Map k b -> (d, M.Map k b)
mapAccumRWithKey f a = second (fmap snd) . mapAccumR f' a . M.mapWithKey (,) where
	f' s (k,x)  = second ((,) k) $ f k x s

touch ::  S.Set (Idx n) -> M.Map (Idx n) (s n)  -> Map s (Succ n) -> Map s (Succ n)
touch touched values  (Map m ms) = let
	(is,m') = mapAccumRWithKey f S.empty m
	f  i (Dep w s js) is 
		| S.null (S.intersection js touched) = (is,Dep w s js)
		| otherwise = 
			let    ss = M.filterWithKey (\k _ -> k `S.member` js) values
			in 	(i `S.insert` is, Dep w (next w ss) js)
	in Map m' $ fmap (touch is (fmap signal m')) ms

update  :: Idx n' -> (Node s n' -> Node s n') -> Map s n -> Map s n
update i@(I _) f (Map m ms) = Map m' (fmap (touch (S.singleton i) (fmap signal m')) ms)
		where 	q Nothing = Nothing
			q (Just x) = Just (f x)
			m' = M.alter q i m

{-

class Interface n n' where
	update  :: Idx n' -> (Node s n' -> Node s n') -> Map s n -> Map s n
	insert :: Node s n' -> Map s n ->  Map s n
	query :: Idx n' -> Map s n -> Maybe (Node s n')

instance Interface n n where
	update i f (Map m ms) = Map m' (fmap (touch (S.singleton i) (fmap signal m')) ms)
		where 	q Nothing = Nothing
			q (Just x) = Just (f x)
			m' = M.alter q i m
	insert x (Map m ms) = Map m' ms
		where 	m' = M.insert (newKey m) x m	
	query i (Map m ms)  = M.lookup i m

instance Interface n n' where
	update i f (Map m (Just ms)) = Map m (Just $ update i f ms)
	update i f (Map m Nothing) = error "updating the impossible"
	insert x (Map m (Just ms)) = Map m (Just $ insert x ms)
	insert x (Map m Nothing) = error "inserting in the impossible"
	query i (Map m (Just ms)) = query i ms
	query i (Map m Nothing) = error "query the impossible"

data S n where
	S0 :: S Zero
	S1 :: S (Succ Zero)

deriving instance Show (S n)

type instance W S n  = ()

instance Signal S Zero where
	next () _ = S1
-}

mS = Map M.empty (Just (Map M.empty Nothing))
