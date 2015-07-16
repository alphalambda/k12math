
import Geometry
import Drawing

main = drawPicture myPicture

myPicture points = 
    drawCircle (a,b) &
    drawLine (a,c) &
    drawPoints [a,b,c] &
    drawLabels [a,b,c] ["A","B","C"] &
    drawPoints intersects &
    drawLabels intersects ["D","E"]
    where [a,b,c] = take 3 points
          intersects = line_circle (a,c) (a,b)