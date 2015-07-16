
module Main where

import Geometry
import Drawing

main = drawPicture myPicture

myPicture points =
    guidelines 2 & xaxis' & yaxis' &
    drawComplex x "x" &
    drawComplex y "y" &
    drawComplex z "z" &
    drawComplex (-x) "-x" &
    drawComplex (-y) "-y" &
    drawComplex (-z) "-z" &
    drawCircle ((0,0),(0,1)) &
    message $ "Complex Numbers"
    where
        o = (0,0)
        x = (-0.5,0.5*s)
        y = (-0.5,-0.5*s)
        z = (1,0)
        s = sqrt 3
        drawComplex x l = drawPointLabel x l & drawSegment (o,x)

(x1,x2) +| (y1,y2) = (x1+y1,x2+y2)

times_i (a,b) = (-b,a)

f rs x = product (map gen rs)
    where gen i = (x - i)/4
          times x = x
          
        
prepare_odd f = (guess (-1) (-1),guess 1 1)
    where
        guess s x | signum (f x) == s = x
                  | otherwise = guess s (2*x)
                  
solve f xneg xpos | abs fmid < 0.00001 = xmid
                  | fmid < 0 = solve f xmid xpos
                  | fmid > 0 = solve f xneg xmid
    where
        xmid = 0.5 * (xneg + xpos)
        fmid = f xmid
        