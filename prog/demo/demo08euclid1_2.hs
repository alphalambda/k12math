
import Geometry
import Drawing

main = drawPicture myPicture

myPicture points =
    red (drawSegment (b,c) & drawPointsLabels [a,b,c] ["A","B","C"] ) &
    faint (drawSegment (a,b) & drawPointLabel o "O"
           & drawSegment (o,a) & drawSegment (o,c')
           & drawPointLabel c' "C'"
           & drawArc (a,b,o) & drawArc (b,a,o)
           & drawArc (c,b,c') & drawArc (c',o,d) ) &
    drawPointLabel d "D" & drawSegment (a,d) &
    message $  "Euclid 1_2     "
               ++ "  AD=" ++ shownum (dist a d)
               ++ ", BC=" ++ shownum (dist b c)
    where { [a,b,c] = take 3 points;
          Just o = find (across c (a,b)) $ circle_circle (a,b) (b,a);
          Just c' = find (beyond (o,b)) $ line_circle (o,b) (b,c);
          Just d = find (beyond (o,a)) $ line_circle (o,a) (o,c');
          }
