
module Main where

import Geometry
import Drawing

main = drawPicture myPicture

myPicture points =
    coordinates' 1 &
    drawComplex x "x" &
    drawComplex y "y" &
    drawComplex z "z" &
    drawComplex x' "x'" &
    drawComplex y' "y'" &
    drawComplex z' "z'" &
    drawSegment (x,z) &
    drawSegment (y,z) &
    drawSegment (x,y) &
    drawSegment (x',z') &
    drawSegment (y',z') &
    drawSegment (x',y') &
    message $ "Complex Numbers"
    where
        o = (0,0)
        [x,y] = take 2 points
        z = x +| y
        x' = times_i x
        y' = times_i y
        z' = times_i z
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
        