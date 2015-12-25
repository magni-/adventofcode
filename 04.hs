import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.Maybe
import Inputs.Input04

--Santa needs help mining some AdventCoins (very similar to bitcoins) to use as gifts for all the economically
--forward-thinking little girls and boys.

--To do this, he needs to find MD5 hashes which, in hexadecimal, start with at least five zeroes. The input to the MD5
--hash is some secret key (your puzzle input, given below) followed by a number in decimal. To mine AdventCoins, you
--must find Santa the lowest positive number (no leading zeroes: 1, 2, 3, ...) that produces such a hash.
parseInput1 :: Int -> Int
parseInput1 number =
  if C.all (== chr 0) firstTwo && ord third < 10
    then number
    else parseInput1 (number + 1)
  where
    (firstTwo, third) = fromJust (C.unsnoc firstThree)
    firstThree = C.take 3 (MD5.hash (C.pack (input ++ show number)))

--Now find one that starts with six zeroes.
parseInput2 :: Int -> Int
parseInput2 number =
  if C.all (== chr 0) firstThree
    then number
    else parseInput2 (number + 1)
  where
    firstThree = C.take 3 (MD5.hash (C.pack (input ++ show number)))

main = do
  putStrLn "--- Day 4: The Ideal Stocking Stuffer ---"
  let number = parseInput1 1
  putStrLn $ "Part 1: " ++ show number
  let number = parseInput2 1
  putStrLn $ "Part 2: " ++ show number
