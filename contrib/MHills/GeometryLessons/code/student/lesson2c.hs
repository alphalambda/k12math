
import Geometry
import Drawing

main = drawPicture myPicture

myPicture points =
    drawCircle (a,b) &
    drawLine (c,d) &
    drawPoints [a,b,c,d] &
    drawLabels [a,b,c,d] ["A","B","C","D"] &
    drawLabels intersects ["X","Y"] &
    drawPoints intersects &
    message "Line-Circle Intersection"
    where [a,b,c,d] = take 4 points
          intersects = line_circle (c,d) (a,b)
          