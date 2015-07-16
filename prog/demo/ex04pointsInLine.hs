
import Drawing
import Exercises

main = drawPicture myPicture

myPicture points =
    drawLine (p,q) &
    drawPointLabel p "P" &
    drawPointLabel q "Q" &
    drawPoints pts &
    drawAutoLabels pts &
    message "Points in a line"
    where list' = take 5 points
          [p,q] = take 2 list'
          list = drop 2 list'
          pts = map (projection (p,q)) list
