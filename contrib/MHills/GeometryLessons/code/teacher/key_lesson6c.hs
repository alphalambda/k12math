
import Drawing
import Geometry
import Geometry.Utils(projection,Triangle(..))

main = drawPicture myPicture

myPicture points =
    coordinates' 2
    & drawPointsLabels [a,b,c,o] ["A","B","C","O"]
    & drawSegment (a,b)
    & drawSegment (b,c)
    & drawSegment (c,a)
    & red (drawSegment (o,c))
    & drawCircle ((0,0),(0,1))
    & messages [ "Triangle"
               , "with base=" ++ shownum (dist a b)
               , "and height=" ++ shownum (dist o c)
               , "Area of ABC is" ++ shownum (area)
               ]
    where
        Triangle a b c = withHeight 4 . withBase 5 . fromPoints . take 3 $ points
        o = projection (a,b) c
        area = 0.5 * dist a b * dist o c