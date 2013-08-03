import Data.List

correct' xs@(x:_) ys = let 
        (ms,us) = partition ((== x) . fst)  ys
        in case ms of 
                [] -> True
                [(_,y)] ->  not (y `elem` xs) && correct' (y:xs) us
                _ -> False

correct x ys = correct' [x] ys
