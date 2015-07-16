
import Drawing
import Exercises
import Geometry

main = drawPicture myPicture

myPicture points =
    red ( drawPointsLabels [a,o,b] ["A","O","B"] &
          drawSegment (a,o) &
          drawSegment (o,b) )
    & faint ( drawArc (a,o,a')
              & drawPointLabel a' "A'"
              & drawSegment (a,a')
              & drawSegment (o,a')
              )
    & drawPointLabel m "M"
    & drawLine (o,m)
    & message $ "Angle Bisector"
              ++ "  angle(AOM)=" ++ shownum (angle a o m)
              ++ ", angle(A'OM)=" ++ shownum (angle a' o m)
    where [a,o,b] = take 3 points
          Just a' = find (not . beyond (b,o)) $ line_circle (o,b) (o,a)
          m = midpoint a a'
