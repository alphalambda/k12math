
import Geometry
import Drawing

main = drawPicture myPicture

myPicture points =
    drawCircle (a,b) &
    drawLine (c,d) &
    drawPoints list &
    drawAutoLabels (list ++ intersects) &
    drawPoints intersects &
    message "Line-Circle Intersection"
    where list = take 4 points
          [a,b,c,d] = list
          intersects = line_circle (c,d) (a,b)
