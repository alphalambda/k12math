
import Drawing
import Exercises
import Geometry

main = drawPicture myPicture

myPicture points =
    message "Incenter of triangle ABC"
    & drawSegment (a,b)
    & drawSegment (b,c)
    & drawSegment (c,a)
    & drawCircle (o,a')
    & faint ( drawSegment (o,a')
            & drawSegment (o,b')
            & drawSegment (o,c')
            )
    & drawPointsLabels [a,b,c] ["A","B","C"]
    & drawPointsLabels [a',b',c'] ["A'","B'","C'"]
    & drawPointLabel o "O"
    where [a,b,c] = take 3 points
          c'' = angle_bisector (b,c,a)
          b'' = angle_bisector (a,b,c)
          a'' = angle_bisector (c,a,b)
          [o] = line_line (a,a'') (b,b'')
          a' = projection (b,c) o
          b' = projection (c,a) o
          c' = projection (a,b) o
          
