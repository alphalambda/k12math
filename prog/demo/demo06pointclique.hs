
import Geometry
import Drawing

main = drawPicture myPicture

myPicture points =
    drawPoints list &
    draw [ drawSegment (a,b) | a <- list, b <- list ] &
    drawAutoLabels list &
    message "Point Clique"
    where list = take 5 points
