
import Drawing

main = drawPicture myPicture

myPicture points = 
    coordinates &
    drawTriangle (a,b,c) &
    drawTriangle (a',b',c') &
    drawLabels [a,b,c,a',b',c'] ["A","B","C","A'","B'","C'"] &
    message "Reflection of ABC over the x-axis"
 
    where 
        [a,b,c] = take 3 points
        a' = dilate a (2,2)
        b' = dilate b (2,2)
        c' = dilate c (2,2)

dilate a (x,y) = a' 
    where (x1,y1) = a
          a' = (x*x1,y*y1)