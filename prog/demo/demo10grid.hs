
module Main where

import Geometry
import Drawing

import qualified Geometry.Utils as U

main = drawPicture myPicture

myPicture = pict

pict points = coordinates
              & blue (drawSegment (U.origin,p)
                    & drawArc (U.unitx,U.origin,p) )
              -- & drawLine (U.origin,p)
              & drawPointLabel p "P"
              & message $ "r=" ++ shownum (dist U.origin p)
                ++ ", t=" ++ shownum (U.phase p) 
                ++ ", P=" ++ showpoint p
        where [p] = take 1 points
