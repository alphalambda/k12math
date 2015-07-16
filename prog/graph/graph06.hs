
module Main where

import Geometry
import Drawing

main = drawPicture myPicture

f x = x*1.004

myPicture points =
    coordinates &
    drawPoints xy &
    message "Exponential"
    where xy = zip range (iterate f 0.0007)
