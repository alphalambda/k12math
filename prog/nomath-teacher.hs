module NoMath where
import Prelude hiding ((+),(-),(*),(/),(^),sqrt,(<),(<=),(>),(>=))

ending num = [last num]

next "0" = "1"; next "1" = "2"; next "2" = "3"; next "3" = "4"; next "4" = "5"
next "5" = "6"; next "6" = "7"; next "7" = "8"; next "8" = "9"; next "9" = "10"

next x | ending x == "9" = next (init x) ++ "0"
       | otherwise = init x ++ next (ending x)

count initial final | initial == final = initial
                    | otherwise = initial ++","++ count (next initial) final

"0" + x = x; x + "0" = x; x + "" = x; "" + x = x

x + y  | init x == "" = prev x + next y           -- x has a single digit
       | init y == "" = next x + prev y           -- y has a single digit
       | otherwise = (init x + init y + init p) ++ ending p
    where p = ending x + ending y

count initial final | initial == final = initial
                    | otherwise = initial ++","++ count (next initial) final

-- Sample run:
-- *NoMath> count "0" "100"
-- "0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,
-- 29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,
-- 55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
-- 81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100"
-- *NoMath> "929" + "741"
-- "1670"

prev "10" = "9"; prev "9" = "8"; prev "8" = "7"; prev "7" = "6"; prev "6" = "5"
prev "5" = "4"; prev "4" = "3"; prev "3" = "2"; prev "2" = "1"; prev "1" = "0"

prev x | ending x == "0" = prev (init x) ++ "9"
       | otherwise = init x ++ prev (ending x)
       

bcount initial final | initial == final = initial
                     | otherwise = initial ++","++ bcount (prev initial) final

separate x = [head x] ++ "," ++ [last x]

double x = x + x

grains "1" = "1"
grains "2" = "2"
grains "3" = "4"
grains "4" = "8"

grains n = double (grains (prev n))

x - "0" = x
x - y = prev x - prev y

x * "0" = "0"
x * "1" = x
x * y = x * prev y + x

x / y | x < y = "0"
      | x == y = "1"
      | otherwise = next $ (x - y) / y
      
x < "0" = False
"0" < x = True
x < y = prev x < prev y    
      
x > y = y < x   

x <= y = x < y || x == y

x >= y = x > y || x == y
