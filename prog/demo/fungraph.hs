
module Main where

import Geometry
import Drawing

main = drawPicture myPicture

myPicture points =
    coordinates &
    drawGraph f1 &
    drawGraph f2 &
    drawGraph f3 &
    drawPointsAs (plus_pt & circle_pt) xy &
    drawGraph (const 3.2) &
    message "Function Graphs"
    where f1 x = -abs (x^2 + 3*x + 2)
          f2 x | x >= 1 = f2 (x-1) + 3
               | x < 0 = f2 (x+1) - 3
               | otherwise = x + 5
          f3 x = x^3 - 2*x^2 + 3*x - 1
          xy = [(x,g x) | x <- [-10..10]]
          g (-10) = 9
          g n = g(n-1) - 0.3
          const a x = a
