
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
    drawSegment (b,d)
    where [a,b] = take 2 points
          [c,d] = circle_circle (a,b) (b,a)
