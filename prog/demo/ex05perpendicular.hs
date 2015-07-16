
import Drawing
import Exercises

main = drawPicture myPicture

myPicture points =
    drawLine (a,b) &
    drawLine (p,q) &
    
    drawPointsLabels [a,b,p,q] ["A","B","P","Q"] &
    
    message $ "Perpendicular to line AB at point B"
    where [a,b] = take 2 points
          (p,q) = perpendicular (a,b)
