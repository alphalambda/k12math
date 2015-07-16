
import Drawing
import Exercises
import Geometry

main = animate myAnimation (drop 3) deciSecs lastFrame
    where
        deciSecs = 19
        lastFrame = 7

myPicture points =
    red ( drawPointsLabels [a,o,b] ["A","O","B"] &
          drawSegment (a,o) &
          drawSegment (o,b) )
    & faint ( drawArc (a,o,a')
              & drawPointLabel a' "A'"
              & drawSegment (a,a')
              & drawSegment (o,a')
              )
    & drawPointLabel m "M"
    & drawLine (o,m)
    & message $ "Angle Bisector"
              ++ "  angle(AOM)=" ++ shownum (angle a o m)
              ++ ", angle(A'OM)=" ++ shownum (angle a' o m)
    where [a,o,b] = take 3 points
          Just a' = find (not . beyond (b,o)) $ line_circle (o,b) (o,a)
          m = midpoint a a'

myAnimation points frame =
    animFromOn frame 1 (red ( drawPointsLabels [a,o,b] ["A","O","B"]
                            & drawSegment (a,o)
                            & drawSegment (o,b) )) &
    animFromOn frame 2 (faint ( drawArc (a,o,a')
                            & drawPointLabel a' "A'"
                            & drawSegment (o,a') )) &
    animFromOn frame 3 (faint (drawSegment (a,a') )) &
    animFromOn frame 4 (drawPointLabel m "M") &
    animFromOn frame 5 (drawLine (o,m)) &
    animFromTo frame 1 5 (message1 frame) &
    animFromOn frame 6 message2
    where [a,o,b] = take 3 points
          Just a' = find (not . beyond (b,o)) $ line_circle (o,b) (o,a)
          m = midpoint a a'
          message1 frame = message $ "Angle Bisector [" ++ show frame ++ "]"
          message2 = message $ "Angle Bisector "
                            ++ "  angle(AOM)=" ++ shownum (angle a o m)
                            ++ ", angle(A'OM)=" ++ shownum (angle a' o m)
          
