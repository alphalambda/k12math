
import Drawing

main = drawPicture myPicture

myPicture points = 
    coordinates &
    drawPoints [a,b,a',b'] &
    drawLabels [a,b,a',b'] ["A","B","A'","B'"] &
    drawSegment (a,b) &
    drawSegment (a',b') &
    message "Reflection of AB over the x-axis"
    
    where 
        [a,b] = [(1,2),(4,2)]
        a' = mirror "x-axis" a
        b' = mirror "x-axis" b
        
mirror "x-axis" a = a'
    where (x,y) = a
          a' = (x,-y)