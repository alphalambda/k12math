
import Geometry
import Drawing

main = drawPicture myPicture

myPicture points =
    drawPoints [a,b,c] &
    drawLabels [a,b,c] ["A","B","C"] &
    message "Points"
    where [a,b,c] = take 3 points
