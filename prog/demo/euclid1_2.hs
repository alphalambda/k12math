
import Geometry
import Drawing

main = drawPicture myPicture

myPicture points =
    drawSegment (b,c) & drawPointsLabels [a,b,c] ["A","B","C"] &
    drawSegment (a,b) & drawPointLabel o "O" &
    drawSegment (o,a) & drawSegment (o,d) &
    drawPointLabel d "D" &
    drawArc (a,b,o) & drawArc (b,a,o) &
    drawArc (c,b,d) & drawArc (d,o,e) &
    drawPointLabel e "E" & drawSegment (a,e) &
    message $  "Euclid 1_2     "
               ++ "  AE=" ++ shownum (dist a e)
               ++ ", BC=" ++ shownum (dist b c)
    where [a,b,c] = take 3 points
          Just o = find (across c (a,b)) $ circle_circle (a,b) (b,a)
          Just d = find (beyond (o,b)) $ line_circle (o,b) (b,c)
          Just e = find (beyond (o,a)) $ line_circle (o,a) (o,d)
