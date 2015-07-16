module Geometry
{-
        ( dist
        , Triangle(..)
        , fromPoints
        , withBase
        , withHeight
        )
-}
where

import qualified Geometry.Solver as S
import qualified Geometry.Utils as U

import Geometry.Utils
    ( Number, Point, Triangle(..)
    {-, badpoint, apart, dist, angle-}
    )

badpoint :: Point -> Bool
badpoint = U.badpoint

-- apart x y tells whether x and y are distinguishable from each other
apart :: Point -> Point -> Bool
apart = U.apart

-- Distance between two points
dist :: Point -> Point -> Number
dist = U.dist

-- Measure of angle ABC (vertex is in the middle)
angle :: Point -> Point -> Point -> Number
angle = U.angle

-- Intersection between lines and circles
--
-- Lines are given by two points
-- Circles are given by the center and a point on the circumference

-- These functions may produce 0, 1 or 2 points in return

circle_circle :: (Point,Point) -> (Point,Point) -> [Point]
circle_circle = S.solve_circles

line_circle :: (Point,Point) -> (Point,Point) -> [Point]
line_circle (a,b) ox | apart a b = S.solve_line_circle (a,b) ox
                     | otherwise = error "line_circle with singleton"

line_line :: (Point,Point) -> (Point,Point) -> [Point]
line_line (a,b) (c,d) | apart a b && apart c d = S.solve_lines (a,b) (c,d)
                        | otherwise = error $ "line_line with singleton"
                                        ++ if not (apart a b) then "(a,b)" else ""
                                        ++ if not (apart c d) then "(c,d)" else ""

--- Betweeness relationship

-- Given 2 points A and B, we can divide the plane into 3 regions
-- by finding perpendicular lines to segment AB passing through A
-- and B respectively. A point X is said to be between A and B if
-- it lies in the middle region.

beyond :: (Point,Point) -> Point -> Bool
beyond = U.beyond

outside :: (Point,Point) -> Point -> Bool
outside (a,b) x = beyond (a,b) x || beyond (b,a) x

-- between (a,b) x tells whether x is between a and b
between :: (Point,Point) -> Point -> Bool
between ab = not . outside ab

sameside :: Point -> (Point,Point) -> Point -> Bool
sameside x (y,y') x' | not a_yy' = error "sameside"
                     | not a_xx' = True
                     | otherwise = case line_line (x,x') (y,y') of
                                    [p] -> not (between (x,x') p)
                                    [] -> True
    where a_yy' = apart y y'
          a_xx' = apart x x'

across x l = not . sameside x l
          
-- [find p pts] returns the first point among [pts] that has property [p]
find = U.find

find_apart p points = case find (apart p) points of
    Just x -> x
    Nothing -> error $ "find_apart " ++ show p

---------------------------------------------------------------------
--- Triangles
---------------------------------------------------------------------

fromPoints :: [Point] -> Triangle
fromPoints = U.fromPoints

withBase :: Number -> Triangle -> Triangle
withBase = U.withBase

withHeight :: Number -> Triangle -> Triangle
withHeight = U.withHeight
