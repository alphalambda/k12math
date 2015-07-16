
import Drawing
import Geometry

main = drawPicture myPicture

myPicture points =
    drawLine (a,b) &
    drawLine (c,d) &
    drawLine (f,g) &
    drawLabels [a,b,c,d,e,f,g] ["A","B","C","D","E","F","G"]&
    message $ "Parallel Lines"
    
    
    where [a,b] = take 2 points
          [c,d] = circle_circle (a,b) (b,a)
          [e] = line_line (a,b) (c,d)
          [f,g] = circle_circle (d,e) (e,d)