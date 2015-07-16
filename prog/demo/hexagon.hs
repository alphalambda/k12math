
import Drawing
import Geometry.Exercises
import Geometry

main = drawPicture myPicture

myPicture points =
    drawPoints [a,b,c,d,e,f] &
    drawLabels [a,b,c,d,e,f] ["A","B","C","D","E","F"] &
    drawPolygon [a,b,c,d,e,f] &
    drawPointLabel o "O" &
    drawSegment (o,a) &
    drawSegment (o,b) &
    drawSegment (o,c) &
    drawSegment (o,d) &
    drawSegment (o,e) &
    drawSegment (o,f) &
    drawArc (b,a,f) &
    drawArc (c,d,e) &
    message "Hexagon"
    where [o,a] = take 2 points
          Just d = find (beyond (a,o)) $ line_circle (o,a) (o,a)
          [b,f] = circle_circle (o,a) (a,o)
          [e,c] = circle_circle (o,d) (d,o)
