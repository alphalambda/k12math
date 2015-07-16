
import Geometry
import Drawing

main = drawPicture myPicture

myPicture points =
    drawPoints [a,b,c,d] &
    drawLabels [a,b,c,d] ["A","B","C","D"] &
    drawSegment (a,b) &
    drawSegment (a,c) &
    drawSegment (a,d) &
    drawSegment (b,c) &
    drawSegment (b,d) &
    drawSegment (c,d) &    
    message "Segments and Lines"
    where [a,b,c,d] = take 4 points
