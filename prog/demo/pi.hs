
import Drawing
import Geometry.Exercises
import Geometry (find,beyond,dist,sameside,line_circle,circle_circle)

main = drawPicture myPicture

myPicture points =
    drawCircle (o,p) &
    drawPoints poly &
    drawPolygon poly &
    message $ "Computing PI=" ++ show (0.5 * perimeter / radius) ++ ", sides=" ++ show num
    where [o,p] = take 2 points
          hex = hexagon o p
          poly = last . take 8 $ iterate (refine o) hex
          num = fromIntegral $ length poly
          perimeter = num * side poly
          radius = dist o p

side (a:b:_) = dist a b

refine o poly = refine' o $ poly ++ [head poly]

refine' _ [a] = []
refine' o (a:b:rest) = (a:a':refine' o (b:rest))
    where m = 0.5 *| (a +| b)
          Just a' = find (beyond (o,m)) $ line_circle (o,m) (o,a)

(x1,y1) +| (x2,y2) = (x1+x2,y1+y2)
t *| (x,y) = (t*x,t*y)
