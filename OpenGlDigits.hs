module OpenGlDigits where

import Graphics.Rendering.OpenGL
import Data.Char

glyphs = [
        [0,1,2,3,4,5]
        , [1,2]
        , [0,1,3,4,6,7]
        , [0,1,2,3,6,7]
        , [1,2,5,6,7]
        , [0,2,3,5,6,7]
        , [0,2,3,4,5,6,7]
        , [0,1,2,5]
        , [0,1,2,3,4,5,6,7]
        , [0,1,2,3,5,6,7]
        , [], [], [], [], [] ,[] ,[]

        ,       [0,1,2,4,5,6,7]
        ,       [0,1,2,3,4,5,6,7]
        ,       [0,3,4,5]
        ,       [0,1,2,3,8,9]
        ,       [0,3,4,5,6]
        ,       [0,4,5,6]
        ,       [0,2,3,4,5,7]
        ,       [1,2,4,5,6,7]
        ,       [8,9]
        ,       [1,2,3]
        ,       [4,5,6,11,12]
        ,       [3,4,5]
        ,       [1,2,4,5,10,12]
        ,       [1,2,4,5,10,11]
        ,       [0,1,2,3,4,5]
        ,       [0,1,4,5,6,7]
        ,       [0,1,2,3,4,5,11]
        ,       [0,1,4,5,6,7,11]
        ,       [0,2,3,5,6,7]
        ,       [0,8,9]
        ,       [1,2,3,4,5]
        ,       [4,5,12,13]
        ,       [1,2,4,5,11,13]
        ,       [10,11,12,13]
        ,       [9,10,12]
        ,       [0,3,12,13]
        ]

renderDigit :: GLfloat -> GLfloat -> Int -> IO ()
renderDigit h' k' n = renderPrimitive Lines $ flip mapM_ (glyphs !! n) $ \l -> do
        let k = realToFrac k'
        let h = realToFrac h'
        case l of 
                0 ->  do
                        vertex (Vertex2 0 1 :: Vertex2 GLfloat)
                        vertex (Vertex2 1 1 :: Vertex2 GLfloat)
                1 ->  do
                        vertex (Vertex2 1 1 :: Vertex2 GLfloat)
                        vertex (Vertex2 1 k :: Vertex2 GLfloat)

                2 ->  do
                        vertex (Vertex2 1 k :: Vertex2 GLfloat)
                        vertex (Vertex2 1 0 :: Vertex2 GLfloat)
                3 ->  do
                        vertex (Vertex2 1 0 :: Vertex2 GLfloat)
                        vertex (Vertex2 0 0 :: Vertex2 GLfloat)
                4 ->  do
                        vertex (Vertex2 0 0 :: Vertex2 GLfloat)
                        vertex (Vertex2 0 k :: Vertex2 GLfloat)
                5 ->  do
                        vertex (Vertex2 0 k :: Vertex2 GLfloat)
                        vertex (Vertex2 0 1 :: Vertex2 GLfloat)
                6 ->  do
                        vertex (Vertex2 0 k :: Vertex2 GLfloat)
                        vertex (Vertex2 h k :: Vertex2 GLfloat)
                7 ->  do
                        vertex (Vertex2 h k :: Vertex2 GLfloat)
                        vertex (Vertex2 1 k :: Vertex2 GLfloat)
                8 ->  do
                        vertex (Vertex2 h 1 :: Vertex2 GLfloat)
                        vertex (Vertex2 h k :: Vertex2 GLfloat)
                9 ->  do
                        vertex (Vertex2 h k :: Vertex2 GLfloat)
                        vertex (Vertex2 h 0 :: Vertex2 GLfloat)
                10 ->  do
                        vertex (Vertex2 0 1 :: Vertex2 GLfloat)
                        vertex (Vertex2 h k :: Vertex2 GLfloat)
                11 ->  do
                        vertex (Vertex2 h k :: Vertex2 GLfloat)
                        vertex (Vertex2 1 0 :: Vertex2 GLfloat)
                12 ->  do
                        vertex (Vertex2 1 1 :: Vertex2 GLfloat)
                        vertex (Vertex2 h k :: Vertex2 GLfloat)
                13 ->  do
                        vertex (Vertex2 h k :: Vertex2 GLfloat)
                        vertex (Vertex2 0 0 :: Vertex2 GLfloat)

renderDigitPosWH :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Int -> IO () 
renderDigitPosWH h' k x y w h n = 
        preservingMatrix $ do
                translate $ (Vector3 (realToFrac x) (realToFrac y) 0 :: Vector3 GLfloat)
                scale (realToFrac w)  (realToFrac h) (1 :: GLfloat)
                renderDigit h' k n
renderNumberPosWH :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Int -> IO ()
renderNumberPosWH h' k x y w h n
        | n < 10 = renderDigitPosWH h k x y w h n
        | otherwise = let
                (d,r) = n `divMod` 10
                in renderDigitPosWH h' k x y w h r >> renderNumberPosWH h' k (x - 1.5*w) y w h d
                
                 
renderWordPOSWH :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> String -> IO ()
renderWordPOSWH h' k x y w h [] = return ()
renderWordPOSWH h' k x y w h (z:zs) = renderDigitPosWH h' k x y w h (ord z - ord '0') >> renderWordPOSWH h' k (x + 1.5*w) y w h zs

