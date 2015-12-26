import qualified Data.ByteString.Char8 as C
import Data.Maybe
import Inputs.Input05
import Text.Regex.PCRE.Light

--Santa needs help figuring out which strings in his text file are naughty or nice.

--A nice string is one with all of the following properties:
--It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
--It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
--It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.

--How many strings are nice?
parseInput1 :: [String] -> Int -> Int
parseInput1 input count = case input of
  [] -> count
  str:input' -> parseInput1 input' (count + (isNice1 str))

isNice1 :: String -> Int
isNice1 str =
  if all isJust [match1, match2] && isNothing match3
    then 1
    else 0
    where
      match1 = match regex1 packedStr []
      match2 = match regex2 packedStr []
      match3 = match regex3 packedStr []
      packedStr = C.pack str
      regex1 = compile (C.pack ".*[aeiou].*[aeiou].*[aeiou].*") []
      regex2 = compile (C.pack "(.)\\1") []
      regex3 = compile (C.pack "ab|cd|pq|xy") []

--Realizing the error of his ways, Santa has switched to a better model of determining whether a string is naughty or
--nice. None of the old rules apply, as they are all clearly ridiculous.

--Now, a nice string is one with all of the following properties:
--It contains a pair of any two letters that appears at least twice in the string without overlapping, like xyxy (xy)
--or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).
--It contains at least one letter which repeats with exactly one letter between them, like xyx, abcdefeghi (efe), or
--even aaa.

--How many strings are nice under these new rules?
parseInput2 :: [String] -> Int -> Int
parseInput2 input count = case input of
  [] -> count
  str:input' -> parseInput2 input' (count + (isNice2 str))

isNice2 :: String -> Int
isNice2 str =
  if all isJust [match1, match2]
    then 1
    else 0
    where
      match1 = match regex1 packedStr []
      match2 = match regex2 packedStr []
      packedStr = C.pack str
      regex1 = compile (C.pack "(..).*\\1") []
      regex2 = compile (C.pack "(.).\\1") []

main = do
  putStrLn "--- Day 5: Doesn't He Have Intern-Elves For This? ---"
  let niceCount = parseInput1 input 0
  putStrLn $ "Part 1: " ++ show niceCount
  let niceCount = parseInput2 input 0
  putStrLn $ "Part 2: " ++ show niceCount
