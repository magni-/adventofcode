import Data.Set
import Inputs.Input03

--Santa is delivering presents to an infinite two-dimensional grid of houses.

--He begins by delivering a present to the house at his starting location, and then an elf at the North Pole calls him
--via radio and tells him where to move next. Moves are always exactly one house to the north (^), south (v), east (>),
--or west (<). After each move, he delivers another present to the house at his new location.

--However, the elf back at the north pole has had a little too much eggnog, and so his directions are a little off, and
--Santa ends up visiting some houses more than once.

--How many houses receive at least one present?
parseInput1 :: [Char] -> (Int, Int) -> Set(Int, Int) -> Int
parseInput1 input (x, y) housesVisited = case input of
  [] -> size housesVisited
  direction:input' -> parseInput1 input' coords (insert coords housesVisited)
    where coords = getNewCoords (x, y) direction

getNewCoords :: (Int, Int) -> Char -> (Int, Int)
getNewCoords (x, y) direction = case direction of
  '<' -> (x - 1, y)
  '^' -> (x, y + 1)
  '>' -> (x + 1, y)
  'v' -> (x, y - 1)

--The next year, to speed up the process, Santa creates a robot version of himself, Robo-Santa, to deliver presents with
--him.

--Santa and Robo-Santa start at the same location (delivering two presents to the same starting house), then take turns
--moving based on instructions from the elf, who is eggnoggedly reading from the same script as the previous year.

--This year, how many houses receive at least one present?
parseInput2 :: [Char] -> Int -> (Int, Int) -> (Int, Int) -> Set(Int, Int) -> Int
parseInput2 input turn (aX, aY) (bX, bY) housesVisited = case input of
  [] -> size housesVisited
  direction:input' -> parseInput2 input' (turn + 1) a' b' (insert newCoords housesVisited)
    where
      (a', b', newCoords) = case (mod turn 2) of
        0 -> (newA, (bX, bY), newA)
        1 -> ((aX, aY), newB, newB)
        where
          newA = (getNewCoords (aX, aY) direction)
          newB = (getNewCoords (bX, bY) direction)

main = do
  putStrLn "--- Day 3: Perfectly Spherical Houses in a Vacuum ---"
  let housesVisited = parseInput1 input (0, 0) (fromList [(0, 0)])
  putStrLn $ "Part 1: " ++ show housesVisited
  let housesVisited = parseInput2 input 0 (0, 0) (0, 0) (fromList [(0, 0)])
  putStrLn $ "Part 2: " ++ show housesVisited
