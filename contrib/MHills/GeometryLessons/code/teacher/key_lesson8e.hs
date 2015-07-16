
import Drawing

main = drawPicture myPicture

myPicture points = 
    coordinates &
    drawPoints [a,b,c,d,a',b',c',d'] &
    drawLabels [a,b,c,d,a',b',c',d'] ["A","B","C","D","A'","B'","C'","D'"] &
    drawSegment (a,b) &
    drawSegment (b,c) &
    drawSegment (c,d) &
    drawSegment (d,a) &
    drawSegment (a',b') &
    drawSegment (b',c') &
    drawSegment (c',d') &
    drawSegment (d',a') &
    message "Rotation of ABCD 180 degrees counter-clockwise"
 
    where 
        [a,b,c,d] = [(1,1),(1,4),(4,4),(4,1)]
        a' = rotate 180 a
        b' = rotate 180 b
        c' = rotate 180 c
        d' = rotate 180 d
        
rotate 90 a = a'
    where (x,y) = a
          a' = (-y,x)
rotate 180 a = a'
    where (x,y) = a
          a' = (-x,-y)
rotate 270 a = a'
    where (x,y) = a
          a' = (y,-x)