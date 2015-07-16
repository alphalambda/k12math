
import Geometry
import Drawing

main = drawPicture myPicture

myPicture points =
    drawPoints [a,b,c,c'] &
    drawLabels [a,b,c,c'] ["A","B","C","C'"] &
    drawLine (a,b)&
    message "Points"
    where [a,b,c] = take 3 points
          c' = find_apart c (drop 3 points)