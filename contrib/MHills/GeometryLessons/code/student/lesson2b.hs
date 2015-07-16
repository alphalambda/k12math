
import Geometry
import Drawing

main = drawPicture myPicture

myPicture points =
    drawLine (a,b) &
    drawLine (c,d) &
    drawPoints [a,b,c,d] &
    drawLabels [a,b,c,d] ["A","B","C","D"] &
    drawPoint p &
    drawLabel p "P" &
    message "Line-Line Intersection"
    where [a,b,c,d] = take 4 points
          [p] = line_line (a,b) (c,d)
