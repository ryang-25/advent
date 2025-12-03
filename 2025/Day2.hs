module Day2 (partOne, partTwo) where

import Debug.Trace
import qualified Data.Text as T

import Utils

-- T.split (uncurry (||) . ((==',') &&& (=='-')) ) x

ceilingTwo :: Int -> Int
ceilingTwo = (*) 2 . ceiling . flip (/) (fromIntegral 2) . fromIntegral

floorTwo :: Int -> Int
floorTwo = (*) 2 . floor . flip (/) (fromIntegral 2) . fromIntegral

nextInvalid :: Int -> Int
nextInvalid n
  | x == y = if right <= left then left * 10^(y `div` 2) + left else (left+1) * 10^(y `div` 2) + (left+1)
  | otherwise = 10^(y-1) + 10^((y-1) `quot` 2)
    where
      (left, right) = quotRem n $ 10^(y `quot` 2)
      x = length $ show n
      y = ceilingTwo x

prevInvalid :: Int -> Int
prevInvalid n
  | x == y = if right >= left then left * 10^(y `div` 2) + left else (left-1) * 10^(y `div` 2) + (left-1)
  | y == 0 = 0
  | otherwise = 10^(y-1) + 10^((y-1) `quot` 2)
    where
      (left, right) = quotRem n $ 10^(y `quot` 2)
      x = length $ show n
      y = floorTwo x

inRange :: Ord a => a -> (a, a) -> Bool
inRange n (x, y) = x <= n && n <= y

isInvalidOne :: Int -> Bool
isInvalidOne n
  | l `rem` 2 /= 0 = False
  | otherwise = uncurry (==) $ quotRem n $ 10^(l `quot` 2)
    where
      l = length $ show n

allEqual :: Eq a => [a] -> Bool
allEqual (x:y:xs) = x == y && allEqual xs
allEqual _ = True

-- isInvalidTwo :: Int -> Bool
-- isInvalidTwo n
--   | 

toRange :: Enum a => [a] -> [a]
toRange (l:h:_) = enumFromTo l h
toRange _ = []


partOne :: [T.Text] -> Int
partOne (x : _) = sum $ map (sum . filter isInvalidOne . toRange . map readT . T.splitOn "-") $ T.splitOn "," x

partTwo :: [T.Text] -> Int
partTwo _ = 1