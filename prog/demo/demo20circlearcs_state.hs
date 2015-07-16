
import Geometry
import Drawing
import Geometry.Utils(Point)
import Control.Monad.State

main = drawPicture myPicture

myPicture points =
    drawCircle (o,a) &
    drawPointsLabels [a,b,p,q] ["A","B","P","Q"] &
    drawSegment (o,a) & drawSegment (o,b) & drawSegment (a,b) &
    drawSegment (p,a) & drawSegment (p,b) &
    drawSegment (o,p) &
    message $ "Circle Arcs"
                ++ " APB=" ++ shownum (angle a p b)
                ++ " AOB=" ++ shownum (angle a o b)
    where [o,x,a,b,p,q] = make points
          make = evalState $ do
            [o,x] <- extract 2 id
            ab <- extract 2 id
            let [a,b] = map (on_circle (o,x)) ab
            [p'] <- extract 1 $ dropWhile (across o (a,b) . on_circle (o,x))
            [q'] <- extract 1 $ dropWhile (sameside o (a,b) . on_circle (o,x))
            let [p,q] = map (on_circle (o,x)) [p',q']
            return [o,x,a,b,p,q]

extract n f = state (splitAt n . f)
            
on_circle (o,x) p = q
    where Just q = find (not . beyond (p,o)) $ line_circle (o,p) (o,x)

