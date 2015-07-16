
import Drawing

main = drawPicture myPicture

myPicture points = 
    coordinates &
    drawPoints [a,b,a',b'] &
    drawLabels [a,b,a',b'] ["A","B","A'","B'"] &
    drawSegment (a,b) &
    message "Translation of AB 3 units right and 1 unit down"
    
    where 
        [a,b] = [(1,1),(2,2)]
        a' = translate a (3,-1)
        b' = translate b (3,-1)
        
translate a (x,y) = a'
    where (x1,y1) = a
          a' = (x1+x,y1+y)