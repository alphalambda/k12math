
module Main where

import Geometry
import Drawing

main = drawPicture myPicture

f x = abs (x^2 + 3*x - 3)

myPicture points =
    coordinates &
    drawGraph f &
    message " f(x)=|x^2+3x-3|"
