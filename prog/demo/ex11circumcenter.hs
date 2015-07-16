
import Drawing
import Exercises
import Geometry

main = drawPicture myPicture

myPicture points =
    message "Circumcenter of triangle ABC"
    & drawSegment (a,b)
    & drawSegment (b,c)
    & drawSegment (c,a)
    & faint ( drawLine a'p
            & drawLine b'p
            & drawLine c'p )
    & drawLabels [a,b,c] ["A","B","C"]
    & drawPointLabel o "O"
    & drawCircle (o,a)
    where [a,b,c] = take 3 points
          c' = midpoint a b
          b' = midpoint a c
          a' = midpoint b c
          a'p = perpendicular (b,a')
          b'p = perpendicular (c,b')
          c'p = perpendicular (a,c')
          [o] = line_line a'p b'p
          
