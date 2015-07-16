
import Drawing
import Geometry.Exercises
import Geometry (find,beyond,line_circle,circle_circle)
import Geometry.Utils (coarse_ne)

main = drawPicture myPicture

tolerance = 4*grain

myPicture points =
    coordinates &
    --drawPluses allpoints &
    --drawPluses somepoints &
    drawPoints diagonals &
    drawPoints hyperbola &
    --drawPoints circle &
    --drawPoints parabola &
    message "Locus"
    where
        allpoints = [(x,y) | x<-range,y<-range ]
        somepoints = [(6,y) | y<-range, y>=6, y<=7 ]
        diagonals = [(x,y) | x<-range,y<-range,
                             abs(abs(x)-abs(y)) < grain ]
        hyperbola = [(x,y) | x<-range,y<-range,
                             abs(x*x-y*y-4) < tolerance ]
        circle = [(x,y) | x<-range,y<-range,
                          abs(x*x+y*y-4) < tolerance ]
        parabola = [(x,y) | x<-range,y<-range,
                            abs(x*x-8-y) < tolerance ]

version1 points =
    drawPoints circle &
    drawPoints parabola &
    message "Fat Circle and Parabola"
    where circle = [(x,y) | x<-range,
                            y<-range,
                            (let r2=x*x+y*y
                             in r2 > 1 && r2 < 4)]
          parabola = [(x,x*x) | x<-range]
