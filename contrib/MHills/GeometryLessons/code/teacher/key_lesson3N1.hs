
import Drawing
import Geometry

main = drawPicture myPicture

myPicture points = 
    coordinates &
    drawPoints [a,b] &
    drawLabels [a,b] ["A","B"] &
    drawPoint m &
    drawLabel m "M" &
    drawSegment (a,b) &
    message ("midpoint = "++ show (m))
    where [a,b] = [(25,10),(50,16)]
          m = midpoint a b
    
midpoint (x1,y1) (x2,y2) = ((x1+x2)/2,(y1+y2)/2)