
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
          a' = mirror (b,c) a
          b' = mirror (c,a) b
          c' = mirror (a,b) c

mirror (b,c) x = x'
    where x' = find_apart x (circle_circle (b,c) (c,b))

    