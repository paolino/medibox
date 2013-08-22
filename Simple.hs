{-# LANGUAGE NoMonomorphismRestriction #-}
module Simple where
        
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Data.Maybe
import Control.Monad
import Control.Lens

import Track
import Score



fill op z = foldr (\(j,x) -> op j .~ Just x ) z . zip [0..]

pas l =  fill dbseq l . map (fill at IM.empty) $ [[Pattern 8 1 0, Pattern 4 1 0, Pattern 2 1 0]
                ,[Pattern 16 1 0, Pattern 4 1 0, Pattern 2 1 0]
                ,[Pattern 16 1 0, Pattern 8 1 0, Pattern 2 1 0]
                ,[Pattern 16 1 0, Pattern 8 1 0, Pattern 4 1 0]
                ,[Pattern 16 1 0, Pattern 8 1 0, Pattern 6 1 0]
                ,[Pattern 16 1 0, Pattern 4 1 0, Pattern 3 1 0]
                ]
prs l = fill dbproj l [       Projection 64 0 0 32 0
                ,       Projection 64 4 0 32 0
                ]

prt l = fill dbtrack l . map (uncurry Track) $ liftM2 (,) [0..5] [0..1]

db = prt . prs . pas $ newDBTrack

{-
        (fill 
        )
        (fill   [       Projection 64 0 0 32 0
                ,       Projection 64 4 0 32 0
                ]
        )
        (fill   []
        )
-}



 
