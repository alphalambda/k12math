
import Drawing
import Exercises
import Geometry (angle)

main = drawPicture myPicture


myPicture points =
    drawPoints [a,b,c,m] &
    drawLine (a,b) &
    drawLine (c,m) &

    drawLabels [a,b,c,m] ["A","B","C","M"] &
    
    message $ "Projection: angle(AMC)=" ++ shownum (angle a m c)
    where [a,b,c] = take 3 points
          m = projection (a,b) c
