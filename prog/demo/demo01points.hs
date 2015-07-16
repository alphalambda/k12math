
import Geometry
import Drawing

main = drawPicture myPicture

myPicture points =
    drawPoints list &
    drawAutoLabels list &
    message "Points"
    where list = take 10 points
