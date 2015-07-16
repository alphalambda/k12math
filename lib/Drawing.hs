module Drawing where

import Graphics.Gloss ( Picture(..) )
import qualified Graphics.Gloss as P
import qualified Graphics.Gloss.Data.Color as Color
import System.Random
import Global

--import qualified Geometry as Geo
import qualified Geometry.Utils as Geo
import Geometry.Utils ( (+|), Number, Point )

(pixelWidth,pixelHeight) = pixelSize
pixelRadius = div (min pixelWidth pixelHeight) 2

pixel2model p = modelSize * fromIntegral p / fromIntegral pixelRadius

bottom = pixel2model $ div (-pixelHeight) 2
left = pixel2model $ div (-pixelWidth) 2
top = pixel2model $ div pixelHeight 2
right = pixel2model $ div pixelWidth 2

dot_radius = 0.1
plot_resolution = 0.1

grain = 0.02

range = [left,next..right]
    where
        left = -4*modelSize
        next = left + grain
        right = -left

infixr 0 &
(&) :: Picture -> Picture -> Picture
a & Pictures bs = Pictures (a:bs)
Pictures [] & b = b
a & b           = Pictures [a,b]

randomPoints gen =
    let (gen1,gen2) = split gen
        hsize = coverage * modelSize
        xs = randomRs (-hsize,hsize) gen1
        ys = randomRs (-hsize,hsize) gen2
    in map Geo.coarsepoint $ zip xs ys

drawPicture generate =
        do gen <- getStdGen
           P.display (P.InWindow windowtitle (pixelWidth,pixelHeight) windowPosition)
                     P.white
                     (P.scale s s $ generate (randomPoints gen))
    where s = fromIntegral pixelRadius / modelSize


animFromOn frame f0 action =
    if frame >= f0 then action else drawNothing

animFromTo frame f0 f1 action =
    if frame >= f0 && frame <= f1 then action else drawNothing

animate userRender userStep deciSecs lastFrame =
        do gen <- getStdGen
           let points = randomPoints gen
           P.simulate (P.InWindow  windowtitle (pixelWidth,pixelHeight) windowPosition)
                      P.white frameRate (deciSecs,0,points) render step
    where
        step _ _ (0,frame,points) | frame == lastFrame = (deciSecs,1,userStep points)
                                  | otherwise = (deciSecs,frame+1,points)
        step _ _ (wait,frame,points) = (wait-1,frame,points)
        s = fromIntegral pixelRadius / modelSize
        render (_,n,points) = P.scale s s $ userRender points n
        frameRate = 10
        
drawGraph f = drawPath $ map f' xs
    where xs = [-margin,-next..margin]
          margin = 2.0 * modelSize
          next = margin - plot_resolution
          f' x = (x,f x)
          
shownum :: Number -> String
shownum x = show . Geo.coarsenum $ x

showpoint :: Point -> String
showpoint (x,y) = "(" ++ shownum x ++ "," ++ shownum y ++ ")"

translate_ (x,y) p = P.translate x y p
rotate_ a = P.rotate (-a)

plus_pt = P.pictures [ hline, vline ]
    where hline = P.line [(-dot_radius,0),(dot_radius,0)]
          vline = P.line [(0,-dot_radius),(0,dot_radius)]

eks_pt = P.pictures [ lline, rline ]
    where lline = P.line [(-dot_radius,-dot_radius),(dot_radius,dot_radius)]
          rline = P.line [(dot_radius,-dot_radius),(-dot_radius,dot_radius)]

star_pt = plus_pt & eks_pt

dot_pt =  P.circleSolid dot_radius

circle_pt = P.circle dot_radius

drawPointAs fig p = translate_ p fig

drawPointsAs fig [] = P.blank
drawPointsAs fig [p] = drawPointAs fig p
drawPointsAs fig (p:ps) = drawPointAs fig p & drawPointsAs fig ps

drawStar = drawPointAs star_pt
drawStars = drawPointsAs star_pt

drawEks = drawPointAs eks_pt
drawEkses = drawPointsAs eks_pt

drawPlus = drawPointAs plus_pt
drawPluses = drawPointsAs plus_pt

drawPoint = drawPointAs dot_pt
drawPoints = drawPointsAs dot_pt

autolabels = [[x] | x <- ['A'..'Z'] ]

draw = P.pictures

drawAutoLabels ps = drawLabels ps autolabels

drawPointLabel p text = drawPoint p & drawLabel p text
drawPointsLabels ps ls = drawPoints ps & drawLabels ps ls

drawLabel p text = translate_ (p +| (0.25,0)) $ P.scale textscale textscale $ P.text text
drawLabels ps ls = P.pictures $ zipWith drawLabel ps ls

drawSegment (a,b) = P.line [a,b]

drawSegmentLabel (pa,pb) (la,lb) =
    drawPointLabel pa la
    & drawPointLabel pb lb
    & P.line [pa,pb]

drawLine (p,q) | Geo.apart p q = P.line [(xlow,ylow),(xhigh,yhigh)]
               | otherwise = P.blank
    where xlow = -(4*modelSize)
          xhigh = 4*modelSize
          ylow = Geo.line_gety l' xlow
          yhigh = Geo.line_gety l' xhigh
          l' = Geo.line (p,q)

-- draw an arc passing through A
drawArc (a,o,b) = translate_ o $ P.arc pstart pstop r
    where a_ = Geo.translate_axes o a
          b_ = Geo.translate_axes o b
          (pstart,pstop) = Geo.dphase a_ b_
          r = Geo.dist a o

drawCircle (o,p) = translate_ o $ P.circle r
    where r = Geo.dist o p

drawCircle' (o,p) = drawPoint o & drawCircle (o,p)

drawCircle'' (o,p) = drawPoint o & drawPoint p & drawCircle (o,p)

drawTriangle (a,b,c) = drawPolygon abc & drawPoints abc
    where abc = [a,b,c]

drawPolygon = P.lineLoop
drawPath = P.line

drawNothing = Pictures []

messageAt (x,y) text = translate_ (x+0.5,y+0.5) $ P.scale textscale textscale $ P.text text

message text = messageAt (left,bottom) text

messages texts = P.pictures . map msg . zip [0..] . reverse $ texts
    where msg (i,text) = messageAt (left,bottom+i) text

strokes _ [] = P.blank
strokes thickness path =
  P.pictures $
    zipWith (stroke thickness) path (tail path)
    ++  map joint path
  where
    joint (x,y) = P.translate x y $ P.circleSolid h
    h = thickness / 2
      
stroke thickness p0 p1 = P.polygon [p0 `vminus` pt, p1 `vminus` pt,
                                    p1 `vplus`  pt, p0 `vplus`  pt]
  where pt = stimes (thickness / 2) p_ortho
        p_ortho = vortho (vunit p0 p1)
        vplus (x0,y0) (x1,y1) = (x0+x1,y0+y1)
        vminus (x0,y0) (x1,y1) = (x0-x1,y0-y1)
        stimes s (x,y) = (s*x,s*y)
        vunit (x0,y0) (x1,y1) = (dx/d, dy/d)
            where
                dx = x1 - x0
                dy = y1 - y0
                d = sqrt (dx*dx + dy*dy)
        vortho (x,y) = (-y,x)

xaxis_ = [(4*left,0),(4*right,0)]
yaxis_ = [(0,4*bottom),(0,4*top)]
xaxis' = P.line xaxis_
yaxis' = P.line yaxis_
xaxis = strokes 0.1 xaxis_
yaxis = strokes 0.1 yaxis_

guidelines step = faint (P.pictures vguides & P.pictures hguides)
    where
        vguides  = map (\x -> P.translate x 0 yaxis') [step,2*step..right']
                ++ map (\x -> P.translate (-x) 0 yaxis') [step,2*step..right']
        hguides  = map (\y -> P.translate 0 y xaxis') [step,2*step..top']
                ++ map (\y -> P.translate 0 (-y) xaxis') [step,2*step..top']
        left' = 4*left
        right' = 4*right
        top' = 4*top
        bottom' = 4*bottom
        
{-
    where xline x y = iline [(-x,0), (x,0)]
          vguidelines = P.pictures [ iline [(x,-vbound),(x,vbound)] | x <- [-hbound, 1-hbound .. hbound] ]
          hguidelines = P.pictures [ iline [(-hbound,y),(hbound,y)] | y <- [-vbound, 1-vbound .. vbound] ]
          iline ps = P.line $ map (\(x,y) -> (fromIntegral x,fromIntegral y)) ps
          hbound = round (4 * size)
          vbound = round (4 * size)
-}

coordinates = xaxis & yaxis & guidelines 1-- & marks

coordinates' step = xaxis & yaxis & guidelines step

red = P.color P.red
blue = P.color P.blue
green = P.color P.green
faint = P.color (Color.makeColor 0.5 0.5 0.5 0.5)

{-
data TurtleState = TurtleState { current :: Point
                               , heading :: Point
                               , pendown :: Bool
                               }

turtle current heading actions = xxx
    where tstate = TurtleState current heading True

-}
