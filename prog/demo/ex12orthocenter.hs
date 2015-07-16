
import Drawing
import Exercises
import Geometry

main = drawPicture myPicture

myPicture points =
    message "Orthocenter of triangle ABC"
    & red (drawSegment (o,a') & drawSegment (a',a) & drawLine (b,c) )
    & green (drawSegment (o,b') & drawSegment (b',b) & drawLine (c,a) )
    & blue (drawSegment (o,c') & drawSegment (c',c) & drawLine (a,b) )
    & drawSegment (a,b)
    & drawSegment (b,c)
    & drawSegment (c,a)
    & drawPointsLabels [a,b,c,a',b',c'] ["A","B","C","A'","B'","C'"]
    & drawPointLabel o "O"
    where [a,b,c] = take 3 points
          c' = projection (a,b) c
          b' = projection (c,a) b
          a' = projection (b,c) a
          [o] = line_line (a,a') (b,b')
          
