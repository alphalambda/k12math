
import Drawing
import Geometry

main = drawPicture myPicture

myPicture points =
    drawLine (a,b) &
    drawLine (c,d) &
    drawLabels [a,b,c,d] ["A","B","C","D"]
    where [a,b] = take 2 points
          [c,d] = circle_circle (a,b) (b,a)