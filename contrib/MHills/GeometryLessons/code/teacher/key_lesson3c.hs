
import Drawing
import Geometry

main = drawPicture myPicture

myPicture points = 
    drawPoints [a,b,c] &
    drawLabels [a,b,c] ["A","B","C"] &
    drawPoints [a',b',c'] &
    drawLabels [a',b',c'] ["A'","B'","C'"] &
    drawSegment (a,b) &
    drawSegment (b,c) &
    drawSegment (c,a) &
    drawSegment (a',b') &
    drawSegment (b',c') &
    drawSegment (a',c')
    where [a,b,c] = take 3 points 
          a' = midpoint b c
          b' = midpoint c a
          c' = midpoint a b
    
midpoint (x1,y1) (x2,y2) = ((x1+x2)/2,(y1+y2)/2)