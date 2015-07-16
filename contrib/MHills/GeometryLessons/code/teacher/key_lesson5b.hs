
import Drawing
import Geometry

main = drawPicture myPicture

myPicture points =
    drawLine (a,b) &
    drawLine (c,d) &
    drawLabels [a,b,c,d,e] ["A","B","C","D","E"]&
    message $ "Perpendicular Lines"
    
    
    where [a,b] = take 2 points
          [c,d] = circle_circle (a,b) (b,a)
          [e] = line_line (a,b) (c,d)