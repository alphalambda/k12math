
module Lessons.Lesson6 where

import Geometry

quadrilateral [a,b,c,d] | across c (a,b) d = (d,b,c,a)
                        | across a (b,c) d = (a,b,d,c)
                        | otherwise = (a,b,c,d)
