module OpenGlDigits where

import Graphics.Rendering.OpenGL

glyphs = [
        [0,1,2,3,4,5]
        , [1,2]
        , [0,1,3,4,6]
        , [0,1,2,3,6]
        , [1,2,5,6]
        , [0,2,3,5,6]
        , [0,2,3,4,5,6]
        , [0,1,2,5]
        , [0,1,2,3,4,5,6]
        , [0,1,2,3,5,6]
        ]

renderDigit :: Double -> Int -> IO ()
renderDigit k' n = renderPrimitive Lines $ flip mapM_ (glyphs !! n) $ \l -> do
        let k = realToFrac k'
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
                        vertex (Vertex2 1 0:: Vertex2 GLfloat)
                        vertex (Vertex2 0 0 :: Vertex2 GLfloat)
                4 ->  do
                        vertex (Vertex2 0 0 :: Vertex2 GLfloat)
                        vertex (Vertex2 0 k :: Vertex2 GLfloat)
                5 ->  do
                        vertex (Vertex2 0 k :: Vertex2 GLfloat)
                        vertex (Vertex2 0 1 :: Vertex2 GLfloat)
                6 ->  do
                        vertex (Vertex2 0 k :: Vertex2 GLfloat)
                        vertex (Vertex2 1 k :: Vertex2 GLfloat)

renderDigitPosWH :: Double -> Double -> Double -> Double -> Double -> Int -> IO () 
renderDigitPosWH k x y w h n = 
        preservingMatrix $ do
                translate $ (Vector3 (realToFrac x) (realToFrac y) 0 :: Vector3 GLfloat)
                scale (realToFrac w)  (realToFrac h) (1 :: GLfloat)
                renderDigit k n
renderNumberPosWH :: Double -> Double -> Double -> Double -> Double -> Int -> IO ()
renderNumberPosWH k x y w h n
        | n < 10 = renderDigitPosWH k x y w h n
        | otherwise = let
                (d,r) = n `divMod` 10
                in renderDigitPosWH k x y w h r >> renderNumberPosWH k (x - 1.5*w) y w h d
                
                 
