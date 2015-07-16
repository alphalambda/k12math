
import Drawing
import Geometry

main = drawPicture myPicture

myPicture points = 
    drawPoints [a,b,c] &
    drawLabels [a,b,c] ["A","B","C"] &
    drawSegment (a,b) &
    drawSegment (b,c) &
    drawSegment (c,a) &
    message ("Perimeter of ABC = "++show(perimeter))
    
    where [a,b,c] = take 3 points
          perimeter = dist a b + dist b c + dist c a