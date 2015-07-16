
module Main where

import Geometry
import Drawing

main = drawPicture myPicture

xy = map move [(x,g x) | x <- [0..100] ]
    where move (x,y) = (x-5,y)

g (0) = 9
g n = g(n-1) - 0.3

myPicture points =
    coordinates &
    drawPoints xy &
    (blue . drawGraph $ const 3.2) &
    message "Arithmetic Sequence"
