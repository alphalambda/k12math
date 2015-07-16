
import Drawing
import Exercises

main = drawPicture myPicture

myPicture points =
    drawSegment (a,b) &
    drawSegment (b,c) &
    drawSegment (c,a) & 
    drawSegment (c,c') &
    drawSegment (b,b') & 
    drawSegment (a,a') &
    drawLabels [a,b,c] ["A","B","C"] &
    drawPointLabel o "O" &
    
    message "Centroid: O is the centroid of triangle ABC"
    where [a,b,c] = take 3 points
          c' = midpoint a b
          b' = midpoint a c
          a' = midpoint b c
          --[centroid] = line_line (c,c') (b,b')
          o = centroid a b c
          
