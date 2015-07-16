
import Drawing

main = drawPicture myPicture

myPicture points = 
    coordinates &
    drawPoints [a,b,a',b'] &
    drawLabels [a,b,a',b'] ["A","B","A'","B'"] &
    drawSegment (a,b) &
    drawSegment (a',b') &
    message "Rotation of AB 90 degrees counter-clockwise"
    
    where 
        [a,b] = [(1,2),(1,4)]
        a' = rotate 90 a
        b' = rotate 90 b
        
rotate 90 a = a'
    where (x,y) = a
          a' = (-y,x)