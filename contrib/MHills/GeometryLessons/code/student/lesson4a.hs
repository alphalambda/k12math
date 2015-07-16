
module Main where

import Geometry
import Drawing

main = drawPicture myPicture

myPicture points = drawTriangle (a,b,c)
                    & drawLabels [a,b,c] ["A","B","C"]
                    & messages [ "angle(ABC)=" ++ shownum (angle a b c)
                               , "angle(BCA)=" ++ shownum (angle b c a)
                               , "angle(CAB)=" ++ shownum (angle c a b)
                               ]
    where [a,b,c] = take 3 points

