
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
    drawSegment (c',a') &
    messages [ "length AB = "++ show (dist a b) 
             , "length BC = "++ show (dist b c) 
             , "length CA = "++ show (dist c a)
             , "length A'B' = "++ show (dist a' b')
             , "length B'C' = "++ show (dist b' c')
             , "length C'A' = "++ show (dist c' a')
             ]
    where [a,b,c] = take 3 points 
          a' = midpoint b c
          b' = midpoint a c
          c' = midpoint a b
    
midpoint (x1,y1) (x2,y2) = ((x1+x2)/2,(y1+y2)/2)