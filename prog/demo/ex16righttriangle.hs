
import Drawing
import Exercises
import Geometry

main = drawPicture myPicture

myPicture points =
    message "Right triangle ABC"
    {- & faint ( drawPointLabel b' "B'"
            & drawSegment (b,b')
            )
    -}
    & drawPointsLabels [a,b,c] ["A","B","C"]
    & drawSegment (a,b)
    & drawSegment (b,c)
    & drawSegment (c,a)
    where [a,b',c] = take 3 points
          b = projection (a,b') c
