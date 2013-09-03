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

pasflat l =  fill dbseq l . map (fill at IM.empty) $ replicate 128 $ [Pattern 8 64 0, Pattern 4 64 0, Pattern 2 64 0, Pattern 0 0 0, Pattern 0 0 0, Pattern 0 0 0, Pattern 0 0 0, Pattern 0 0 0]



pas l =  fill dbseq l . map (fill at IM.empty) $ [[Pattern 8 64 0, Pattern 4 64 0, Pattern 2 64 0, Pattern 0 0 0, Pattern 0 0 0, Pattern 0 0 0, Pattern 0 0 0, Pattern 0 0 0]
                ,[Pattern 16 64 0, Pattern 4 64 0, Pattern 2 64 0, Pattern 0 0 0, Pattern 0 0 0, Pattern 0 0 0, Pattern 0 0 0, Pattern 0 0 0]
                ,[Pattern 16 64 0, Pattern 8 64 0, Pattern 2 64 0, Pattern 0 0 0, Pattern 0 0 0, Pattern 0 0 0, Pattern 0 0 0, Pattern 0 0 0]
                ,[Pattern 16 64 0, Pattern 8 64 0, Pattern 4 64 0, Pattern 0 0 0, Pattern 0 0 0, Pattern 0 0 0, Pattern 0 0 0, Pattern 0 0 0]
                ,[Pattern 16 64 0, Pattern 8 64 0, Pattern 6 64 0, Pattern 0 0 0, Pattern 0 0 0, Pattern 0 0 0, Pattern 0 0 0, Pattern 0 0 0]
                ,[Pattern 16 64 0, Pattern 4 64 0, Pattern 3 64 0, Pattern 0 0 0, Pattern 0 0 0, Pattern 0 0 0, Pattern 0 0 0, Pattern 0 0 0]
                ]
prsflat l =   fill dbproj l $ replicate 128 $ Projection 64 4 64 0 32 0

prs l = fill dbproj l [       Projection 64 0 64 0 32 0
                ,       Projection 64 4 64 0 32 0
                ]

prtflat l = fill dbtrack l . map (uncurry Track) $ zip [0..127] [0..127]

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

dbflat = prtflat . prsflat . pasflat $ newDBTrack

 
