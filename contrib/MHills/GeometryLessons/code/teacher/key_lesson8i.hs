
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
        a' = mirror "x-axis" a
        b' = mirror "x-axis" b
        c' = mirror "x-axis" c
        
mirror "x-axis" a = a'
    where (x,y) = a
          a' = (x,-y)