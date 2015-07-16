module Geometry.Utils where

import qualified Data.List as L

type Number = Float
type Point = (Number,Number)

eps :: Number
eps = 0.01

coarsenum x = approx
    where approx = eps * fromIntegral roundup
          roundup = round (x / eps)

coarse_gt :: Number -> Number -> Bool
coarse_gt x y = x-y > eps

coarse_lt :: Number -> Number -> Bool
coarse_lt x y = y-x > eps

coarse_ne :: Number -> Number -> Bool
coarse_ne x y = coarse_lt x y || coarse_gt x y

coarsepoint (x,y) = (coarsenum x,coarsenum y)

apart (x1,y1) (x2,y2) = coarse_ne x1 x2 || coarse_ne y1 y2

badpoint (x,y) = isNaN x || isNaN y

origin :: Point
origin = (0.0,0.0)

unitx :: Point
unitx = (1.0,0.0)

unity :: Point
unity = (0.0,1.0)

(x1,y1) +| (x2,y2) = (x1+x2,y1+y2)
(x1,y1) -| (x2,y2) = (x1-x2,y1-y2)

s *| (x,y) = (s*x,s*y)
(x,y) /| s = (x/s,y/s)
(x1,y1) .| (x2,y2) = x1*x2 + y1*y2

vec :: Point -> Point -> Point
vec (px,py) (qx,qy) = (qx-px,qy-py)

neg :: Point -> Point
neg (x,y) = (-x,-y)

ort :: Point -> Point
ort (p,q) = (-q,p)

norm :: Point -> Number
norm (x,y) = sqrt (x*x + y*y)

uvec :: Point -> Point -> Point
uvec p q = pq /| norm pq
    where pq = vec p q

midpoint :: Point -> Point -> Point
midpoint p q = 0.5 *| (p +| q)

projection :: (Point,Point) -> Point -> Point
projection (a,b) x = a +| ( (vec a x .| u) *| u )
    where u = uvec a b
    
dist :: Point -> Point -> Number
dist (x1,y1) (x2,y2) = sqrt (dx*dx + dy*dy)
    where dx = x2 - x1
          dy = y2 - y1

-- (x_,y_) coordinates when axes are rotated and x_ axis passes through (a,b)
rotate_axes (a,b) (x,y) = (x_,y_)
    where x_ = (a*x+b*y)/r
          y_ = (-b*x+a*y)/r
          r = sqrt (a*a+b*b)

-- (x_,y_) coordinates when axes are translated parallel to 
-- the original with the new center at (a,b)
translate_axes :: Point -> Point -> Point
translate_axes (a,b) (x,y) = (x-a,y-b)

-- line in standard format : ax + by = c
-- should also be normalized
newtype StdLine = StdLine (Number,Number,Number)

normalize a b c | c < 0.0 = normalize (-a) (-b) (-c)
                | n > 0.0 = StdLine (a',b',c')
                | otherwise = error $ "normalize:"
                        ++ " a=" ++ show a
                        ++ " b=" ++ show b
                        ++ " c=" ++ show c
    where n = sqrt (a*a + b*b)
          a' = (a/n)
          b' = (b/n)
          c' = (c/n)

line :: (Point,Point) -> StdLine
line (p,q) | apart p q = normalize a b c
           | otherwise = error "Line with singleton"
    where r = vec p q
          (a,b) = ort r
          c = (a,b) .| p

translate_axes_line (p,q) (StdLine (a,b,c)) = normalize a b (c-a*p-b*q)

line_gety :: StdLine -> Number -> Number
line_gety (StdLine (a,b,c)) x | b /= 0.0 = (c - a*x) / b

dist_point_line p l = c
    where (StdLine (_,_,c)) = translate_axes_line p l

type Circle = (Point,Point)

phase :: Point -> Number
phase (x,y) | y >= 0 = rawphase
            | otherwise = rawphase + 360.0
    where rawphase = atan2 y x * 180.0 / pi

-- p1 - p2 (mod 360)
angle_sub p1 p2 | d < 0 = 360 + d
                | otherwise = d
    where d = p1 - p2

-- compute inner angle and give the start and stop phases
-- in counterclockwise direction
dphase a b | pos <= neg = (p1,p2)
           | otherwise = (p2,p1)
    where p1 = phase a
          p2 = phase b
          pos = angle_sub p2 p1 -- counterclockwise angle
          neg = angle_sub p1 p2 -- clockwise angle
          
-- inner angle measure
angle a o b | d >  180 = 360 - d
            | otherwise = d
    where p1 = phase $ vec o a
          p2 = phase $ vec o b
          d = abs (p2 - p1)

beyond (a,b) c = angle a b c > 90

find = L.find

---------------------------------------------------------------------
--- Triangles
---------------------------------------------------------------------

data Triangle = Triangle Point Point Point

fromPoints :: [Point] -> Triangle
fromPoints [a,b,c] = Triangle a b c

withBase :: Number -> Triangle -> Triangle
withBase base (Triangle a b c) = Triangle a' b' c
    where
        o = projection (a,b) c
        s = base / dist a b
        a' = o +| (s *| vec o a)
        b' = o +| (s *| vec o b)

withHeight :: Number -> Triangle -> Triangle
withHeight height (Triangle a b c) = Triangle a b c'
    where
        o = projection (a,b) c
        s = height / dist o c
        c' = o +| (s *| vec o c)
