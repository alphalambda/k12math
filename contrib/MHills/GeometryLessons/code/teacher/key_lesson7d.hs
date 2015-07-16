
import Drawing
import Geometry

main = drawPicture myPicture

myPicture points =
    
    drawPoints [a,b,c,c',b',a'] &
    drawLabels [a,b,c,c',b',a'] ["A","B","C","C'","B'","A'"] &
    drawSegment (a,b) &
    drawSegment (b,c) &
    drawSegment (c,a) &
	drawSegment (a,b') &
	drawSegment (c,b') &
	drawSegment (c,a') &
	drawSegment (a',b) &
	drawSegment (a,c') &
	drawSegment (b,c')
    
    where [a,c] = take 2 points
          [b,x] = circle_circle (a,c) (c,a)
          b' = find_apart b [b,x]
          [m,n] = circle_circle (c,b) (b,c)
          a' = find_apart a [m,n]
          [e,d] = circle_circle (a,b) (b,a)
          c' = find_apart c [e,d]


