
import Drawing
import Geometry

main = drawPicture myPicture

myPicture points =
    drawPoints [a,b,c,d] &
    drawLabels [a,b,c,d] ["A","B","C","D"] &
    drawSegment (a,b) &
    drawSegment (b,c) &
    drawSegment (c,a) &
    drawSegment (a,d) &
    drawSegment (c,d)
    where [a,c] = take 2 points
          [b,d] = circle_circle (a,c) (c,a)