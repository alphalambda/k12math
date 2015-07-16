
import Drawing
import Exercises

main = drawPicture myPicture

myPicture points =
    drawLine (a,b) &
    drawLine (p,q) &
    drawLine (r,s) &
    
    drawPointsLabels [a,b,c,d] ["A","B","C","D"] &
    
    message $ "Parallel lines"
    where [a,b,c,d] = take 4 points
          c' = projection (a,b) c
          (p,q) = perpendicular (c',c)
          (r,s) = parallel (a,b) d
