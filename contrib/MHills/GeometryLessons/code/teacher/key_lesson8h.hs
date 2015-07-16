
import Drawing

main = drawPicture myPicture

myPicture points = 
    coordinates &
    drawPoints [a,b,c,d,a',b',c,d'] &
    drawLabels [a,b,c,d,a',b',c',d'] ["A","B","C","D","A'","B'","C'","D'"] &
    drawSegment (a,b) &
    drawSegment (b,c) &
    drawSegment (c,d) &
    drawSegment (d,a) &
    drawSegment (a',b') &
    drawSegment (b',c') &
    drawSegment (c',d') &
    drawSegment (d',a') &
    message "Reflection of ABCD over the y-axis"
    
    where 
        [a,b,c,d] = [(1,2),(1,4),(3,4),(3,2)]
        a' = mirror "y-axis" a
        b' = mirror "y-axis" b
        c' = mirror "y-axis" c
        d' = mirror "y-axis" d
        
mirror "x-axis" a = a'
    where (x,y) = a
          a' = (x,-y)
mirror "y-axis" a = a'
    where (x,y) = a
          a' = (-x,y)