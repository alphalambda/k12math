
import Geometry
import Drawing
import Exercises

main = drawPicture myPicture

myPicture points =
    drawPolygon [a,b,d,c] &
    drawLabels [a,b,c,d] ["A","B","C","D"] &
    
    message $ "Parallelogram"
    where [a,b,c] = take 3 points
          d = translation (a,b) c
