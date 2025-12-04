{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day2 (partOne, partTwo) where

import qualified Data.Text as T

import Utils

digits :: Int -> Int
digits = (+1) . floor . logBase 10 . fromIntegral

toRange :: Enum a => [a] -> [a]
toRange (l:h:_) = enumFromTo l h
toRange _ = []

parseRanges :: T.Text -> [[Int]]
parseRanges = map (toRange . map readT . T.splitOn "-") . T.splitOn ","

partOne :: [T.Text] -> Int
partOne (x : _) = sum $ map (sum . filter isInvalid) $ parseRanges x
  where
    isInvalid n
      | l `rem` 2 /= 0 = False
      | otherwise = uncurry (==) $ quotRem n $ 10^(l `quot` 2)
      where
        l = digits n

partTwo :: [T.Text] -> Int
partTwo (x : _) = sum $ map (sum . filter isInvalid) $ parseRanges x
  where
      isInvalid n = or [same $ chunks i (show n) | i <- [1 .. l `quot` 2], l `rem` i == 0]
        where
          l = digits n
