
import Drawing
import Exercises
import Geometry (find,beyond,line_circle,circle_circle)
import Geometry.Utils (coarse_ne)

main = drawPicture myPicture

myPicture = version1

{--
grain = 0.02

left = -10
next = left + grain
right = -left

--}

version1 points =
    drawPoints circle &
    drawPoints parabola &
    message "Fat Circle and Parabola"
    where circle = [(x,y) | x<-range,
                            y<-range,
                            (let r2=x*x+y*y
                             in r2 > 1 && r2 < 4)]
          parabola = [(x,x*x) | x<-range]
