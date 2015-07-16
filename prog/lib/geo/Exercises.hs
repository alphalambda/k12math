module Exercises where

import Geometry
import qualified Drawing as D

find_beyond l = find (beyond l)

find_across p (q,r) = find (not . sameside p (q,r))

in_triangle (a,b,c) x = away (x,a') (a,b) && away (x,a') (a,c)
    && away (x,b') (b,c) && away (x,b') (a,b)
    && away (x,c') (a,c) && away (x,c') (b,c)
    where c' = midpoint a b
          b' = midpoint c a
          a' = midpoint b c

{-
are_parallels (a,b) (c,d) = null (line_line (a,b) (c,d)) && apart a a'
    where p = perp (a,b)
          a' = line_line p (c,d)
-}
          
away (a,b) (c,d) = case line_line (a,b) (c,d) of
    [] -> True
    [x] -> outside (a,b) x

hexagon o a = [a,b,c,d,e,f]
    where Just d = find (beyond (a,o)) $ line_circle (o,a) (o,a)
          [b,f] = circle_circle (o,a) (a,o)
          [e,c] = circle_circle (o,d) (d,o)

midpoint a b | apart a b = m
             | otherwise = a
    where intersect = circle_circle (a,b) (b,a)
          [m] = line_line (a,b) (c,d)
          [c,d] = case intersect of
                    [c,d] -> [c,d]
                    [] -> error ("Null midpoint"
                                 ++ ", a=" ++ D.showpoint a
                                 ++ ", b=" ++ D.showpoint b)

centroid a b c = m
    where c' = midpoint a b
          b' = midpoint a c
          a' = midpoint b c
          [m] = line_line (c,c') (b,b')

projection (a,b) c | apart a b = c'
                   | otherwise = error "Projection not apart"
    where inter = line_circle (a,b) (c,a) 
          c' = case inter of
                    [d,e] -> midpoint d e
                    [c'] -> c'
                    _ -> error ("Projection: "
                                ++ ",a=" ++ D.showpoint a
                                ++ ",b=" ++ D.showpoint b
                                ++ ",c=" ++ D.showpoint c)
                    --other -> a

perpendicular (a,b) | apart a b = (c,d)
                    | otherwise = error $ "perpendicular:"
                                    ++ D.showpoint a
    where Just a' = find (beyond (a,b)) $ line_circle (a,b) (b,a)
          [c,d] = circle_circle (a,a') (a',a)

parallel (a,b) c = perpendicular (c',c)
    where c' = projection (a,b) c

translation (a,b) c = d
    where ab' = parallel (a,b) c
          ac' = parallel (a,c) b
          [d] = line_line ab' ac'

angle_bisector (a,o,b) = m
    where Just a' = find (not . beyond (b,o)) $ line_circle (o,b) (o,a)
          m = midpoint a a'

segment_bisector (a,b) = (p,m,q)
    where m = midpoint a b
          (p,q) = perpendicular (a,m)

transport (b,c) a = d
    where Just o = find_across c (a,b) $ circle_circle (a,b) (b,a)
          Just c' = find_beyond (o,b) $ line_circle (o,b) (b,c)
          Just d = find_beyond (o,a) $ line_circle (o,a) (o,c')

transport_beyond (p,q) (a,b) = c
    where q' = transport (p,q) b
          Just c = find_beyond (a,b) $ line_circle (a,b) (b,q')

circumcenter a b c = o
    where a' = midpoint b c
          a'p = perpendicular (b,a')
          b' = midpoint a c
          b'p = perpendicular (c,b')
          --c' = midpoint a b
          --c'p = perpendicular (a,c')
          [o] = line_line a'p b'p

orthocenter a b c = o
    where a' = projection (b,c) a
          b' = projection (c,a) b
          --c' = projection (a,b) c
          [o] = line_line (a,a') (b,b')

incenter a b c = o
    where a'' = angle_bisector (c,a,b)
          b'' = angle_bisector (a,b,c)
          --c'' = angle_bisector (b,c,a)
          [o] = line_line (a,a'') (b,b'')

mirror (a,b) x = x'
    where m = projection (a,b) x
          Just x' = find_across x (a,b) $ line_circle (m,x) (m,x)

rotation (a,o,b) x = x'
    where Just a' = find (not . beyond (b,o)) $ line_circle (o,b) (o,a)
          bs = angle_bisector (a,o,a')
          x' = mirror (o,a') $ mirror (o,bs) x
