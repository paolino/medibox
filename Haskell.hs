module Haskell where

unzip4 = foldr f ([],[],[],[]) where
		f (x,y,z,g) (xs,ys,zs,gs) = (x:xs,y:ys,z:zs,g:gs)
unzip5 = foldr f ([],[],[],[],[]) where
		f (x,y,z,g,k) (xs,ys,zs,gs,ks) = (x:xs,y:ys,z:zs,g:gs,k:ks)
unzip6 = foldr f ([],[],[],[],[],[]) where
		f (x,y,z,g,k,w) (xs,ys,zs,gs,ks,ws) = (x:xs,y:ys,z:zs,g:gs,k:ks,w:ws)
unzip7 = foldr f ([],[],[],[],[],[],[]) where
		f (x,y,z,g,k,w,r) (xs,ys,zs,gs,ks,ws,rs) = (x:xs,y:ys,z:zs,g:gs,k:ks,w:ws,r:rs)

