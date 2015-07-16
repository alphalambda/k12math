
import Drawing
import Exercises
import Geometry

main = drawPicture myPicture

myPicture points =
    red ( drawPointsLabels [a,b] ["A","B"]
          & drawSegment (a,b)
          )
    & faint ( drawPointsLabels [p,q] ["P","Q"] )
    & drawPointLabel m "M"
    & drawLine (p,q)
    & message $ "Segment Bisector"
              ++ "  AM=" ++ shownum (dist a m)
              ++ ", MB=" ++ shownum (dist m b)
    where [a,b] = take 2 points
          m = midpoint a b
          (p,q) = perpendicular (a,m)

