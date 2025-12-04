module Day3 (partOne, partTwo) where

import Control.Arrow (first)
import Data.Char (digitToInt)
import Data.List (delete, find, unsnoc)
import Data.Maybe (fromJust)

import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Text as T

tupleToInt :: (Int, Int) -> Int
tupleToInt (a, b) = a*10 + b

unpackDigits :: T.Text -> [Int]
unpackDigits =  map digitToInt . T.unpack

partOne :: [T.Text] -> Int
partOne = sum . map (tupleToInt . maxJolts . unpackDigits)
  where
    maxJolts_ (a, b) c
      | c > a = (c, -1)
      | c > b = (a, c)
      | otherwise = (a, b)
    handleLast (a, b) c = if c > b then (a, c) else (a, b)
    maxJolts xs = case f xs of
        (_, -1) -> uncurry handleLast $ first f $ fromJust $ unsnoc xs
        _ -> f xs
      where
          f  = foldl maxJolts_ (-1, -1)

liftMaybe :: (Maybe a, b) -> Maybe (a, b)
liftMaybe ((Just x), y) = Just (x, y)
liftMaybe _ = Nothing

deleteFind :: Eq a => (a -> Bool) -> [a] -> Maybe (a, [a])
deleteFind p xs = (\x -> (x, delete x xs)) <$> find p xs

-- n is the largest index bound
indexOfMax :: Ord a => a -> IntMap [a] -> Maybe (IntMap.Key, IntMap [a])
indexOfMax n = liftMaybe . first (fst <$>) . IntMap.mapAccumRWithKey f Nothing
  where
    f Nothing k xs = case deleteFind (<=n) xs of
      Just (x, xs') -> (Just (k, x), xs')
      Nothing -> (Nothing, xs)
    f (Just (k, i)) _ xs = (,) (Just (k, i)) $ dropWhile (<i) xs

intFromList :: [Int] -> Int
intFromList = foldr (\(n, k) -> (+ n * 10^k)) 0 . flip zip [(0 :: Int)..] . reverse

-- should be O(n) :)
partTwo :: [T.Text] -> Int
partTwo = sum . map (maxJolts . unpackDigits)
  where
    indices xs = IntMap.map reverse $ IntMap.fromListWith (++) $ zip xs $ map (\x -> [x]) [0..]
    maxJolts_ n l m
      | n == l = []
      | otherwise = case indexOfMax n m of
          Just (k, m') -> k : maxJolts_ (n+1) l m'
          Nothing -> []
    maxJolts xs = intFromList $ maxJolts_ (l - 12) l $ indices xs
      where
        l = length xs
