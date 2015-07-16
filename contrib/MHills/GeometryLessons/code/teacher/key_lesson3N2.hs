
import Drawing
import Geometry

main = drawPicture myPicture

myPicture points = 
    coordinates &
    drawPoints [a,b] &
    drawLabels [a,b] ["A","B"] &
    drawSegment (a,b) &
    message ("length AB = "++ show (dist a b))
    where [a,b] = [(25,10),(50,16)]

   