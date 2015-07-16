module Geometry.Solver where

import Geometry.Utils

-------------------------------
--- Quadratic equation
-------------------------------

-- find the solutions to the standard quadratic equation: ax^2 + bx + c = 0
solve_quadratic a b c | d == 0 = [t]
                      | d > 0 = [t+h,t-h]
                      | otherwise = []
    where d = b*b - 4*a*c
          t = -b/(2*a)
          h = sqrt d / (2*a)

-------------------------------
--- Line-Line Intersections
-------------------------------

-- find the intersection of lines given by standard form: ax+by = c
solve_lines_standard (a1,b1,c1) (a2,b2,c2) | coarse_ne d 0.0 = [(x,y)]
                                           | otherwise = []
    where d = a1*b2 - a2 * b1
          x = (c1*b2 - c2*b1) / d
          y = (a1*c2 - a2*c1) / d
          
solve_lines l1 l2 = solve_lines_standard abc1 abc2
    where (StdLine abc1) = line l1
          (StdLine abc2) = line l2

-------------------------------
--- Line-Circle Intersections
-------------------------------

solve_line_centeredcircle (a,b,c) r | coarse_gt c r = []
                                    | coarse_lt c r = [proj_point +| perp, proj_point -| perp]
                                    | otherwise = [proj_point]
    where proj_point = (c*a,c*b)
          perp = d *| ort (a,b)
          d = if r*r >= c*c then sqrt (r*r - c*c)
              else error $ "Bad sqrt in solve_line"
                    ++ " r=" ++ show r
                    ++ " c=" ++ show c

solve_line_circle l (o,p) = map (translate_axes $ neg o) pts
    where (StdLine abc) = translate_axes_line o (line l)
          r = dist o p
          pts = solve_line_centeredcircle abc r

-------------------------------
--- Circle-Circle Intersections
-------------------------------

radius_cond cmp r r1 r2 =
    cmp r (r1 + r2) && cmp r1 (r + r2) && cmp r2 (r + r1)

{-
radius_lt r r1 r2 =
    cmp r (r1 + r2) && cmp r1 (r + r2) && cmp r2 (r + r1)
    where cmp = coarse_lt

-}

radius_gt r r1 r2 =
    cmp r (r1 + r2) || cmp r1 (r + r2) || cmp r2 (r + r1)
    where cmp = coarse_gt

-- easy case: circle of radius r1 at (0,0) and circle of radius r2 at (r,0)
solve_circles_easy r1 r2 r | radius_cond coarse_lt r r1 r2 = [(x,y1),(x,y2)]
                           | radius_gt r r1 r2 = []
                           | coarse_gt r 0.0 = [(x,0)]
                           | otherwise = [(0,0)]
    where x = (r*r + r1*r1 - r2*r2) / 2/r
          y1 = sqrt (r1*r1 - x*x)
          y2 = -y1
                                 
-- medium case: circle r1 at (0,0) and circle r2 at (a,b)
solve_circles_medium r1 r2 (a,b) =
    case solve_circles_easy r1 r2 (sqrt (a*a+b*b)) of
        [p_] -> [rotate_axes (a,-b) p_]
        [p1_,p2_] -> [rotate_axes (a,-b) p1_, rotate_axes (a,-b) p2_]
        other -> other

-- general case
solve_circles_hard r1 (a,b) r2 (c,d) =
    case solve_circles_medium r1 r2 (c-a,d-b) of
        [(x,y)] -> [(x+a,y+b)]
        [(x1,y1),(x2,y2)] -> [(x1+a,y1+b),(x2+a,y2+b)]
        other -> other

-- solve when circles are given by center and point
solve_circles (o1,p1) (o2,p2) = solve_circles_hard r1 o1 r2 o2
    where r1 = dist o1 p1
          r2 = dist o2 p2
