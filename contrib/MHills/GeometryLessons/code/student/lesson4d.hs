
module Main where

import Geometry
import Drawing

main = drawPicture myPicture

myPicture points = drawTriangle (a,b,c)
                    & drawLabels [a,b,c] ["A","B","C"]
                    & messages [ "angle(ABC)=" ++ shownum (angle a b c)
                               , "angle(BCA)=" ++ shownum (angle b c a)
                               , "angle(CAB)=" ++ shownum (angle c a b)
                               , "The triangle is " ++ analyze a b c
                               ]
    where [a,b,c] = take 3 points

analyze a b c | angle a b c > 90 = "obtuse"
              | angle b c a > 90 = "obtuse"
              | angle c a b > 90 = "obtuse"
              | otherwise = "not obtuse"