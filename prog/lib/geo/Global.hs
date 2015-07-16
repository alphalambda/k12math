
module Global where

-- The proportion (in length) of the area where points are generated
coverage :: Float
coverage = 0.5 

-- The relative size of the text
textscale :: Float
textscale = 0.005

-- The window title
windowtitle = "Geometry"

-- The size of the window in pixels
pixelSize :: (Int,Int)
pixelSize = (800,600)

-- The initial position of the window in the screen
windowPosition :: (Int,Int)
windowPosition = (0,0)

-- The model coordinates of the shown area
modelSize :: Float
modelSize = 10.0
