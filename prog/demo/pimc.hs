
import Drawing
import Geometry.Exercises
import Geometry (find,beyond,dist,sameside,line_circle,circle_circle)

main = drawPicture myPicture

myPicture points =
    drawCircle (o,p) &
    drawPoints good &
    message $ "Computing PI=" ++ show (4 * approx_pi4)
              ++ "   using " ++ show nsamples ++ " samples"
    where o = (0,0)
          p = (5,0)
          nsamples = 100000
          samples = take nsamples points
          good = [(x,y) | (x,y) <- samples, x*x+y*y <= 25 ]
          area = fromIntegral (length good)
          total = fromIntegral (length samples)
          approx_pi4 = area/total;
