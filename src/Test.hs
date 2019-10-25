
import Data.Maybe

candidates :: Int -> [Int] -- generate candidate perfect numbers
candidates bound = takeWhile (<= bound) $ scanl1 (+) [1..] -- This will greatly reduce the set of candidate perfect numbers.

maxFactor :: Int -> Int
maxFactor m | even m    = m `div` 2 -- half way is as far as you will get for a successful factor.
            | otherwise = m `div` 3 -- odd perfects can have their candidate factors reduced more.

isFactor :: Int -> Int -> Bool
isFactor c f = mod c f == 0

perfects :: Int -> [Int]
perfects n =
  catMaybes
  [let s = sum $ filter (isFactor target) [1.. maxFactor target]
   in if s == target
      then Just s
      else Nothing
   | target <- candidates n
  ]
