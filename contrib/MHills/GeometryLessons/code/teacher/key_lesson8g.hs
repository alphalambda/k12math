
import Drawing

main = drawPicture myPicture

myPicture points = 
    coordinates &
    drawPoints [a,b,c,a',b',c] &
    drawLabels [a,b,c,a',b',c'] ["A","B","C","A'","B'","C'"] &
    drawSegment (a,b) &
    drawSegment (b,c) &
    drawSegment (c,a) &
    drawSegment (a',b') &
    drawSegment (b',c') &
    drawSegment (c',a') &
    message "Reflection of ABC over the x-axis"
    
    where 
        [a,b,c] = [(1,2),(4,2),(3,3)]
        a' = mirror "x-axis" a
        b' = mirror "x-axis" b
        c' = mirror "x-axis" c
        
mirror "x-axis" a = a'
    where (x,y) = a
          a' = (x,-y)