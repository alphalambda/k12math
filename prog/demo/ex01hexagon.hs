
import Drawing
import Exercises
import Geometry (find,beyond,line_circle,circle_circle)

main = drawPicture myPicture

myPicture = version1

version1 points =
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
    message "Hexagon"
    where list = take 2 points
          [o,a] = list
          Just d = find (beyond (a,o)) $ line_circle (o,a) (o,a)
          [b,f] = circle_circle (o,a) (a,o)
          [e,c] = circle_circle (o,d) (d,o)

version2 points =
    drawPoints hex &
    drawPolygon hex &
    drawAutoLabels hex &
    message "Hexagon"
    where [o,p] = take 2 points
          hex = hexagon o p
