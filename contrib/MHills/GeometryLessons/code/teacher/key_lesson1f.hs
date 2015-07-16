
import Geometry
import Drawing

main = drawPicture myPicture

myPicture points =
    drawPoints [a,b,c] &
    drawLabels [a,b,c] ["A","B","C"] &
    drawSegment (a,b) &
    drawLine (b,c) &
    drawSegment (c,a)&
    message "Segments and Lines"
    where [a,b,c] = take 3 points
