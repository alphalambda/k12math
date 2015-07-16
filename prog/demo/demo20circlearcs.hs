
import Geometry
import Drawing
import Data.List(partition)

main = drawPicture myPicture

myPicture points =
    drawCircle (o,a) &
    drawPointsLabels [a,b,p,q] ["A","B","P","Q"] &
    red (drawSegment (o,a) & drawSegment (o,b) & drawSegment (a,b)) &
    blue (drawSegment (o,p) & drawSegment (o,q) & drawSegment (p,q)) &
    message $ "Circle Arcs"
                ++ " APB=" ++ shownum (angle a p b)
                ++ " AOB=" ++ shownum (angle a o b)
    where [o,x] = take 2 points
          circlepts = map (on_circle (o,x)) . drop 2 $ points
          [a,b] = take 2 circlepts
          (acrosspts,samepts) = partition (across o (a,b)) . drop 2 $ circlepts
          q = head acrosspts
          p = head samepts

on_circle (o,x) p = q
    where Just q = find (not . beyond (p,o)) $ line_circle (o,p) (o,x)

