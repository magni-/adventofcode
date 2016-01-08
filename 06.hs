import Control.DeepSeq
import Data.List.Split
import qualified Data.Vector as V
import Inputs.Input06

--Because your neighbors keep defeating you in the holiday house decorating contest year after year, you've decided to
--deploy one million lights in a 1000x1000 grid.

--Furthermore, because you've been especially nice this year, Santa has mailed you instructions on how to display the
--ideal lighting configuration.

--Lights in your grid are numbered from 0 to 999 in each direction; the lights at each corner are at 0,0, 0,999,
--999,999, and 999,0. The instructions include whether to turn on, turn off, or toggle various inclusive ranges given
--as coordinate pairs. Each coordinate pair represents opposite corners of a rectangle, inclusive; a coordinate pair
--like 0,0 through 2,2 therefore refers to 9 lights in a 3x3 square. The lights all start turned off.

--To defeat your neighbors this year, all you have to do is set up your lights by doing the instructions Santa sent you
--in order.

--After following the instructions, how many lights are lit?
parseInput1 :: [String] -> Int
parseInput1 input = length (V.filter (== on) (V.concat (V.toList lights)))
  where lights = setUpLights input (V.replicate 1000 (V.replicate 1000 off))

type Light = Bool
type Grid a = V.Vector (V.Vector a)
data Action = Toggle | TurnOn | TurnOff
data Corner = Corner { x :: Int, y :: Int }
type Rectangle = (Corner, Corner)
data Instruction = Instruction { action :: Action, rectangle :: Rectangle }

on = True :: Light
off = False :: Light

toggle :: Light -> Light
toggle = not

turnOn :: Light -> Light
turnOn light = on

turnOff :: Light -> Light
turnOff light = off

setUpLights :: [String] -> Grid Light -> Grid Light
setUpLights instructions lights = case instructions of
  [] -> lights
  instruction:instructions' -> setUpLights instructions' $!! lights'
    where
      lights' = doInstruction lights instruction

parseInstruction :: String -> Instruction
parseInstruction str = case action of
  "toggle" -> Instruction Toggle rectangle
  "on" -> Instruction TurnOn rectangle
  "off" -> Instruction TurnOff rectangle
  where
    maxXY:"through":minXY:action:_ = reverse (words str)
    rectangle = (parseCorner minXY, parseCorner maxXY)

parseCorner :: String -> Corner
parseCorner str = Corner (head coordList) (last coordList)
  where coordList = map (read :: String -> Int) (splitOn "," str)

doInstruction :: Grid Light -> String -> Grid Light
doInstruction lights str = case action instruction of
  Toggle -> changeLights' toggle
  TurnOn -> changeLights' turnOn
  TurnOff -> changeLights' turnOff
  where
    instruction = parseInstruction str
    isInRectangle' i j = isInRectangle (rectangle instruction) i j
    changeLights' = changeLights lights isInRectangle'

isInRectangle :: Rectangle -> Int -> Int -> Bool
isInRectangle (bottomLeft, topRight) x' y' =
  x bottomLeft <= x' && x' <= x topRight && y bottomLeft <= y' && y' <= y topRight

changeLights :: Grid Light -> (Int -> Int -> Bool) -> (Light -> Light) -> Grid Light
changeLights lights isInRectangle action =
  V.imap (\i -> \row -> V.imap (\j -> \light -> if isInRectangle i j then action light else light) row) lights

--You just finish implementing your winning light pattern when you realize you mistranslated Santa's message from
--Ancient Nordic Elvish. The light grid you bought actually has individual brightness controls; each light can have a
--brightness of zero or more. The lights all start at zero.

--The phrase turn on actually means that you should increase the brightness of those lights by 1.
--The phrase turn off actually means that you should decrease the brightness of those lights by 1, to a minimum of zero.
--The phrase toggle actually means that you should increase the brightness of those lights by 2.

--What is the total brightness of all lights combined after following Santa's instructions?
parseInput2 :: [String] -> Int
parseInput2 instructions = sum (V.concat (V.toList brightnessGrid))
  where brightnessGrid = setBrightness input (V.replicate 1000 (V.replicate 1000 0))

setBrightness :: [String] -> Grid Int -> Grid Int
setBrightness instructions brightnessGrid = case instructions of
  [] -> brightnessGrid
  instruction:instructions' -> setBrightness instructions' $!! brightnessGrid'
    where
      brightnessGrid' = doBrightnessInstruction brightnessGrid instruction

increase :: Int -> Int -> Int
increase amount initial = initial + amount

decrease :: Int -> Int -> Int
decrease amount initial = max (initial - amount) 0

doBrightnessInstruction :: Grid Int -> String -> Grid Int
doBrightnessInstruction brightnessGrid str = case action instruction of
  Toggle -> changeBrightness' increase 2
  TurnOn -> changeBrightness' increase 1
  TurnOff -> changeBrightness' decrease 1
  where
    instruction = parseInstruction str
    isInRectangle' i j = isInRectangle (rectangle instruction) i j
    changeBrightness' = changeBrightness brightnessGrid isInRectangle'

changeBrightness :: Grid Int -> (Int -> Int -> Bool) -> (Int -> Int -> Int) -> Int -> Grid Int
changeBrightness brightnessGrid isInRectangle tweak amount =
  V.imap (\i -> \row -> V.imap (\j -> \brightness -> if isInRectangle i j then tweak amount brightness else brightness) row) brightnessGrid

main = do
  putStrLn "--- Day 6: Probably a Fire Hazard ---"
  let lightCount = parseInput1 input
  putStrLn $ "Part 1: " ++ show lightCount
  let brightness = parseInput2 input
  putStrLn $ "Part 2: " ++ show brightness
