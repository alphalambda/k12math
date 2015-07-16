
import Drawing
import Exercises
import Geometry

main = drawPicture myPicture

myPicture points =
    message ("Rotations" ++ ", angle(AOA')=" ++ shownum (angle a o a')
                         ++ ", angle(XOX')=" ++ shownum (angle x o x'))
    & red ( drawSegment (o,a)
          & drawSegment (a,x)
          & drawSegment (x,o)
          )
    & blue ( drawSegment (o,a')
           & drawSegment (a',x')
           & drawSegment (o,x')
           )
    & faint ( drawArc (x,o,x')
            & drawArc (a,o,a')
            )
    & drawPointsLabels [o,a,a',x,x'] ["O","A","A'","X","X'"]
    where [a,o,b,x] = take 4 points
          Just a' = find (not . beyond (b,o)) $ line_circle (o,b) (o,a)
          bs = angle_bisector (a,o,a')
          x' = mirror (o,a') $ mirror (o,bs) x
