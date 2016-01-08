import Data.List.Split
import Inputs.Input02

--The elves are running low on wrapping paper, and so they need to submit an order for more. They have a list of the
--dimensions (length l, width w, and height h) of each present, and only want to order exactly as much as they need.

--Fortunately, every present is a box (a perfect right rectangular prism), which makes calculating the required
--wrapping paper for each gift a little easier: find the surface area of the box, which is 2*l*w + 2*w*h + 2*h*l. The
--elves also need a little extra paper for each present: the area of the smallest side.

--All numbers in the elves' list are in feet. How many total square feet of wrapping paper should they order?
parseInput1 :: [[Char]] -> Int -> Int
parseInput1 input total = case input of
  [] -> total
  dimensionString:input' -> parseInput1 input' (total + packageTotalArea)
    where
     packageTotalArea = getTotalArea packageSideAreas
      where
       packageSideAreas = getSideAreas dimensions
          where
            dimensions = getDimensions dimensionString

getDimensions :: [Char] -> (Int, Int, Int)
getDimensions input = (dimensionsArray !! 0, dimensionsArray !! 1, dimensionsArray !! 2)
  where dimensionsArray = map (read :: String -> Int) (splitOn "x" input)

getTotalArea :: (Int, Int, Int) -> Int
getTotalArea (x, y, z) = 2 * x + 2 * y + 2 * z + smallestSideArea
  where smallestSideArea = (min (min x y) z)

getSideAreas :: (Int, Int, Int) -> (Int, Int, Int)
getSideAreas (l, w, h) = (l * w, w * h, h * l)

--The elves are also running low on ribbon. Ribbon is all the same width, so they only have to worry about the length
--they need to order, which they would again like to be exact.

--The ribbon required to wrap a present is the shortest distance around its sides, or the smallest perimeter of any one
--face. Each present also requires a bow made out of ribbon as well; the feet of ribbon required for the perfect bow is
--equal to the cubic feet of volume of the present. Don't ask how they tie the bow, though; they'll never tell.

--How many total feet of ribbon should they order?
parseInput2 :: [[Char]] -> Int -> Int
parseInput2 input total = case input of
  [] -> total
  dimensionString:input' -> parseInput2 input' (total + packageRibbonLength)
    where
      packageRibbonLength = getSmallestPerimeter dimensions + getPackageVolume dimensions
      dimensions = getDimensions dimensionString

getSmallestPerimeter :: (Int, Int, Int) -> Int
getSmallestPerimeter (l, w, h) = 2 * (min (min (l + w) (w + h)) (h + l))

getPackageVolume :: (Int, Int, Int) -> Int
getPackageVolume (l, w, h) = l * w * h

main = do
  putStrLn "--- Day 2: I Was Told There Would Be No Math ---"
  let totalSurfaceArea = parseInput1 input 0
  putStrLn $ "Part 1: " ++ show totalSurfaceArea
  let totalRibbonLength = parseInput2 input 0
  putStrLn $ "Part 2: " ++ show totalRibbonLength
