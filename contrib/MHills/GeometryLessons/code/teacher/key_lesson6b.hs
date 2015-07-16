
import Drawing
import Lessons.Lesson6
import Geometry

main = drawPicture myPicture

myPicture points = 
    drawPoints [a,b,c,d] &
    drawLabels [a,b,c,d] ["A","B","C","D"] &
    drawSegment (a,b) &
    drawSegment (b,c) &
    drawSegment (c,d) &
    drawSegment (d,a) &
    messages [ " AB = " ++show(dist a b)
             ,  " BC = " ++show(dist b c)
             ,  " CD = " ++show(dist c d)
             ,  " DA = " ++show(dist b a)
             ,  " Perimeter of ABCD = "++show(perimeter)
             ]  
    
    where (a,b,c,d) = quadrilateral (take 4 points)
          perimeter = dist a b + dist b c + dist c d + dist d a