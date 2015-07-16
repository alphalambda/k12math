
import Geometry
import Drawing

main = drawPicture myPicture

myPicture points =
    drawLine (a,b) &
    drawLine (c,d) &
    drawPoints list &
    drawAutoLabels list &
    drawPoint p &
    drawLabel p "P" &
    message "Line-Line Intersection"
    where list = take 4 points
          [a,b,c,d] = list
          [p] = line_line (a,b) (c,d)
