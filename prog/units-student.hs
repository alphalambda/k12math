
module UnitConversion where

fl_oz = 1  -- This is our basic unit of volume measurement

-- These are derivative units:
gallon = 4 * quart
quart = 2 * pint
pint = 2 * cup
cup = 8 * fl_oz
tbsp = 1/16 * cup
tsp = 1/3 * tbsp

-- Problem 1: How many tsp are in 2 pints?
solution1 = undefined

-- Problem 2: In general, how many units are in some amount?
convert_to unit amount = undefined

