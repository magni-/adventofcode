import Inputs.Input01

--Santa is trying to deliver presents in a large apartment building, but he can't find the right floor - the directions he
--got are a little confusing. He starts on the ground floor (floor 0) and then follows the instructions one character at a time.

--An opening parenthesis, (, means he should go up one floor, and a closing parenthesis, ), means he should go down one floor.
--The apartment building is very tall, and the basement is very deep; he will never find the top or bottom floors.

--To what floor do the instructions take Santa?
parseInput1 :: [Char] -> Int -> Int
parseInput1 input floor = case input of
  [] -> floor
  '(':input' -> parseInput1 input' (floor + 1)
  ')':input' -> parseInput1 input' (floor - 1)

--Now, given the same instructions, find the position of the first character that causes him to enter the basement (floor -1).
--The first character in the instructions has position 1, the second character has position 2, and so on.

--What is the position of the character that causes Santa to first enter the basement?
parseInput2 :: [Char] -> Int -> Int -> Int
parseInput2 input floor position = case floor of
  -1 -> position
  _ -> parseInput2 (tail input) nextFloor (position + 1)
    where nextFloor = if head input == '(' then floor + 1 else floor -1

main = do
  putStrLn "--- Day 1: Not Quite Lisp ---"
  let floor = parseInput1 input 0
  putStrLn $ "Part 1: " ++ show floor
  let position = parseInput2 input 0 0
  putStrLn $ "Part 2: " ++ show position
