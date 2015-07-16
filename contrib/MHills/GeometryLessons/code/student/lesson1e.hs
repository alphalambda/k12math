
import Geometry
import Drawing

main = drawPicture myPicture


myPicture points =
    drawLine (a,b) &
    drawLine (b,c) &
    drawLine (c,a) &
    drawPoints [a,b,c] &
    drawAutoLabels [a,b,c] &
    message "Lines"
    where [a,b,c] = take 3 points
