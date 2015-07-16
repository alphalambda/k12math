
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
    message "Translation of ABCD 2 units right and 5 units up"
    
    where 
        [a,b,c,d] = [(0,0),(3,0),(3,3),(0,3)]
        a' = translate a (2,5)
        b' = translate b (2,5)
        c' = translate c (2,5)
        d' = translate d (2,5)
        
translate a (x,y) = a'
    where (x1,y1) = a
          a' = (x1+x,y1+y)