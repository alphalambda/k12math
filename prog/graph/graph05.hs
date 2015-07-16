
module Main where

import Geometry
import Drawing

main = drawPicture myPicture

f x | x >= 1 = f (x-1) + 3
    | x < 0  = f (x+1) - 3
    | otherwise = x + 5

myPicture points =
    coordinates &
    drawGraph f &
    message "Function Graph"
