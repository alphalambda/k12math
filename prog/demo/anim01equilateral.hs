
import Drawing
import Geometry

main = animate myAnimation (drop 2) deciSecs lastFrame
    where
        deciSecs = 15
        lastFrame = 9

myPicture points =
    drawPoints [a,b,c] &
    drawLabels [a,b,c] ["A","B","C"] &
    drawSegment (a,b) &
    drawSegment (b,c) &
    drawSegment (c,a)
    where
        [a,b] = take 2 points
        [c,d] = circle_circle (a,b) (b,a)

myAnimation points frame =
    animFromOn frame 1   (drawPointsLabels [a,b] ["A","B"]) &
    animFromOn frame 2   (drawSegment (a,b)) &
    animFromTo frame 3 7 (drawCircle (a,b)) &
    animFromTo frame 4 7 (drawCircle (b,a)) &
    animFromOn frame 5   (drawPointLabel c "C") &
    animFromOn frame 6   (drawSegment (b,c)) &
    animFromOn frame 7   (drawSegment (c,a)) &
    animFromTo frame 1 7
        (message $ "Animated Equilateral Drawing [" ++ show frame ++ "]") &
    animFromOn frame 8 (message "Animated Equilateral Drawing")
    where
        [a,b] = take 2 points
        [c,d] = circle_circle (a,b) (b,a)
