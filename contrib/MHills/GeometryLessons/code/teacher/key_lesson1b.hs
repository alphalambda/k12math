
import Geometry
import Drawing

main = drawPicture myPicture

myPicture points =
    drawPoints [a,b,c,d,e,f] &
    drawLabels [a,b,c,d,e,f] ["A","B","C","D","E","F"] &
    message "Points"
    where [a,b,c,d,e,f] = take 6 points
