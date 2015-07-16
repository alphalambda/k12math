
import Geometry
import Drawing

main = drawPicture myPicture

myPicture points =
    red ( drawCircle'' (a,b) ) &
    blue ( drawCircle'' (c,d) ) &
    drawLabels [a,b,c,d] ["A","B","C","D"] &
    drawLabels intersects ["X","Y"] &
    drawPoints intersects &
    message "Circle-Circle Intersection"
    where [a,b,c,d] = [(0,0),(5,5),(0,0),(1,1)]
          intersects = circle_circle (c,d) (a,b)
