
module Main where

import Geometry
import Drawing

main = drawPicture myPicture

f rs x = product (map gen rs)
    where gen i = (x - i)/5
          times x = x
          
myPicture points =
    coordinates' 5 &
    blue (drawGraph g) &
    message $ "A root is " ++ show root ++ ", f(root)=" ++ show (g root)
    where
        g = f $ map fst (take 11 points)
        root = solve g xneg xpos
        (xneg,xpos) = prepare_odd g
        
prepare_odd f = (guess (-1) (-1),guess 1 1)
    where
        guess s x | signum (f x) == s = x
                  | otherwise = guess s (2*x)
                  
solve f xneg xpos | abs fmid < 0.00001 = xmid
                  | fmid < 0 = solve f xmid xpos
                  | fmid > 0 = solve f xneg xmid
    where
        xmid = 0.5 * (xneg + xpos)
        fmid = f xmid
        