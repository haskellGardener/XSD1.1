{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
import Prelude
import Data.Maybe
import System.Random
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef

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

pairs :: [Int] -> [Int] -> [(Int,Int)]
pairs m p = filter (\(a,b) -> a == b) $ zip m p


-- | Randomly shuffle a list without the IO Monad
--   /O(N)/
shuffle' :: [a] -> StdGen -> ([a],StdGen)
shuffle' xs gen = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- liftM (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray' n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen'))
  where
    n = length xs
    newArray' :: Int -> [a] -> ST s (STArray s Int a)
    newArray' n' xs' =  newListArray (1,n') xs'

shuffleIO :: [a] -> IO [a]
shuffleIO xs = getStdRandom (shuffle' xs)

avgInt :: [Int] -> Double
avgInt is = fromIntegral (sum is) / fromIntegral (length is)


class Booleanizable a where
    toBoolean :: a -> Bool

instance (Num a, Eq a) => Booleanizable a where
    toBoolean 0 = False
    toBoolean _ = True

