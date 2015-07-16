
import Geometry
import Drawing

main = drawPicture myPicture

myPicture points =
    red ( drawCircle'' (a,b) ) &
    blue ( drawCircle'' (c,d) ) &
    drawAutoLabels (list ++ intersects) &
    drawPoints intersects &
    message "Circle-Circle Intersection"
    where list = take 4 points
          [a,b,c,d] = list
          intersects = circle_circle (c,d) (a,b)
