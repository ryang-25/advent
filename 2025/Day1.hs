{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day1 (partOne, partTwo) where

import qualified Data.Text as T

import Utils

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)

partOne :: [T.Text] -> Int
partOne = count 0 . scanl zeros 50
  where
    zeros n ('L' T.:< s) = mod (n - readT s) 100
    zeros n ('R' T.:< s) = mod (n + readT s) 100

partTwo :: [T.Text] -> Int
partTwo = fst . foldl' zeros (0, 50)
  where
    zeros :: (Int, Int) -> T.Text -> (Int, Int)
    zeros (n, r) (d T.:< s) = (n + abs n' + b, r')
      where
        op = if d == 'L' then (-) else (+)
        (n', r') = divMod (r `op` readT s) 100
        -- if we rotate left we undercount by 1 unless we start at zero
        b = if (d == 'L' && r' == 0) then 1 else 0 + if (d == 'L' && r == 0) then -1 else 0
