
import Drawing
import Exercises
import Geometry

main = drawPicture myPicture

myPicture points =
    red ( drawPointsLabels [a,b,c,d] ["A","B","C","D"]
          & drawSegment (a,b)
          & drawSegment (b,c)
          & drawSegment (c,d)
          )
    & faint ( drawCircle (b,a)
              & drawCircle (c,d)
              )
    & draw (map (\x -> drawSegment (b,x) & drawSegment (c,x)) inter)
    & drawPointsLabels inter ["X","Y"]
    & message $ "Construct triangle with lengths"
                ++ ": AB=" ++ shownum (dist a b)
                ++ ", BC=" ++ shownum (dist b c)
                ++ ", CD=" ++ shownum (dist c d)
    where [a,b,c',d'] = take 4 points
          Just c = find_beyond (a,b) $ line_circle (a,b) (b,c')
          d = transport_beyond (c',d') (a,c)
          inter = circle_circle (b,a) (c,d)
