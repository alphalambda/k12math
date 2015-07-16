
import Drawing
import Geometry

main = drawPicture myPicture

myPicture points = 
    drawPoints [a,b,c] &
    drawLabels [a,b,c] ["A","B","C"] &
    drawPoint a' &
    drawLabel a' "A'" &
    drawSegment (a,b) &
    drawSegment (b,c) &
    drawSegment (c,a)
    where [a,b,c] = take 3 points 
          a' = midpoint b c
    
midpoint (x1,y1) (x2,y2) = ((x1+x2)/2,(y1+y2)/2)