
module Main where

import Geometry
import Drawing

main = drawPicture myPicture

myPicture points =
    coordinates &
    drawGraph f &
    drawGraph g &
    red (drawPoints f_pts) &
    blue (drawPoints g_pts) &
    drawPoints fg_pts
    where samples = take 5000 points
          f x = 2*x+3
          g x = 5-3*x
          fg_pts = [(x,y) | (x,y) <- samples,
                            y > f x,
                            y < g x]
          f_pts = [(x,y) | (x,y) <- samples,
                           y > f x ]
          g_pts = [(x,y) | (x,y) <- samples,
                           y < g x ]
