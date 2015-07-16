
module Main where

import Geometry
import Drawing

main = drawPicture myPicture

myPicture points =
    coordinates &
    red (drawPoints xy1) &
    blue (drawPoints xy2)
    where xy1 = [(x,y) | (x,y) <- samples, abs (x - 2) > 4 ]
          xy2 = [(x,y) | (x,y) <- samples, abs (x^2 - 1) < 0.3 ]
          samples = take 10000 points