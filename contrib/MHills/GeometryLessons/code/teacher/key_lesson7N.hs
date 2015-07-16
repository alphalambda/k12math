
import Drawing
import Geometry

main = drawPicture myPicture

myPicture points =
    coordinates &
    drawPoints [a,b,c] &
    drawLabels [a,b,c] ["A","B","C"] &
    drawPoints [a',b',c'] &
    drawLabels [a',b',c'] ["A'","B'","C'"] &
    drawSegment (a,b) &
    drawSegment (b,c) &
    drawSegment (c,a) &
    messages ["height=" ++ show (dist c a')
             , "c is at " ++ show(c)
             ]
    
    where [a,b] = [(-5,0),(5,0)]
          [c,d] = circle_circle (a,b) (b,a)
          a' = midpoint b c
          b' = midpoint a c
          c' = midpoint a b 

midpoint (a1,a2) (b1,b2) = ((a1+b1)/2,(a2+b2)/2) 
         