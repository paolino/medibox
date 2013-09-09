module GL2D where

import Graphics.Rendering.OpenGL (GLfloat)

-- | 2D Vector for OpenGL
type Point = (GLfloat, GLfloat)

-- | 2D Operations, vector sum, difference and biscaling
(.+.),(.-.),(.*.) :: Point -> Point -> Point

(x,y) .+. (x1,y1) = (x + x1,y + y1)
(x,y) .-. (x1,y1) = (x - x1,y - y1)
(x,y) .*. (k,h) = (x*k,y*h)

-- | 2D euclidean distance
distance :: Point -> Point -> GLfloat
distance (x,y) (x1,y1) = sqrt ((x - x1) ^ 2 + (y - y1) ^ 2)

-- | midpoint
mid :: GLfloat -> Point -> Point -> Point
mid k (x,y) (x1,y1) = ((x + k*x1) / (k + 1) ,(y + k * y1) / (k + 1))

