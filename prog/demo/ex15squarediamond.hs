
import Drawing
import Exercises
import Geometry

main = drawPicture myPicture

myPicture points =
    red ( drawSquare a b c d ) &
    blue ( drawDiamond a b' c d' ) &
    drawSegment (b,d) &
    message $ "Square and Diamond"
    where [a,b,x] = take 3 points
          d = head $ line_circle (perpendicular (b,a)) (a,b)
          c = translation (a,b) d
          b' = projection (b,d) x
          Just d' = find_across b' (a,c) $ circle_circle (a,b') (c,b')
          
drawSquare a b c d =
    drawSegment (a,b) &
    drawSegment (b,c) &
    drawSegment (c,d) & 
    drawSegment (d,a) &
    drawLabels [a,b,c,d] ["A","B","C","D"]
          
drawDiamond a b' c d' =
    drawSegment (a,b')
    & drawSegment (b',c)
    & drawSegment (c,d')
    & drawSegment (d',a)
    -- & drawLabels [a,b',c,d'] ["A","B'","C","D'"]
    
