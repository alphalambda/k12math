
import Geometry
import Drawing

main = drawPicture myPicture

myPicture points =
    drawPoints [a,b,c,d,e] &
    drawLabels [a,b,c,d,e] ["A","B","C","D","E"] &
    drawSegment (a,b) &
    drawSegment (a,c) &
    drawSegment (a,d) &
    drawSegment (a,e) &
    drawSegment (b,c) &
    drawSegment (b,d) &
    drawSegment (b,e) &
    drawSegment (c,d) &
    drawSegment (c,e) &
    drawSegment (d,e) &
    message "Segments and Lines"
    where [a,b,c,d,e] = take 5 points
