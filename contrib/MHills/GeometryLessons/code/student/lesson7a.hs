
import Drawing
import Geometry

main = drawPicture myPicture

myPicture points =
    drawPoints [a,b,c] &
    drawLabels [a,b,c] ["A","B","C"] &
    drawSegment (a,b) &
    drawSegment (b,c) &
    drawSegment (c,a)
    where [a,b] = take 2 points
          [c,d] = circle_circle (a,b) (b,a)
