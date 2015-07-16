
import Geometry
import Drawing
import Exercises

main = drawPicture myPicture

myPicture points =
    drawTriangle (a,b,c) &
    drawTriangle (b,c,d) &
    drawTriangle (c,d,e) &
    drawTriangle (d,e,f) &
    drawTriangle (e,f,g) &
    drawAutoLabels list &
    message "Triangle strip : triangles sharing a side"
    where list = reverse . fst .
                 addpoint . addpoint . addpoint . addpoint
                 $ splitAt 3 points
          [a,b,c,d,e,f,g] = list

{-
in_triangle (a,b,c) x = away (x,a') (a,b) && away (x,a') (a,c)
    && away (x,b') (b,c) && away (x,b') (a,b)
    && away (x,c') (a,c) && away (x,c') (b,c)
    where c' = midpoint a b
          b' = midpoint c a
          a' = midpoint b c
          
away (a,b) (c,d) = case line_line (a,b) (c,d) of
    [] -> True
    [x] -> outside (a,b) x
-}

addpoint (list,points) = (list',points')
    where [c,b,a] = take 3 list
          ([x],points') = splitAt 1 (dropWhile (in_triangle (a,b,c)) points)
          list' = x:list
