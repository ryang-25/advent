{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day1 (partOne, partTwo) where

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)

partOne :: [String] -> Int
partOne = count 0 . scanl zeros 50
  where
    zeros n ('L' : s) = mod (n - read s) 100
    zeros n ('R' : s) = mod (n + read s) 100

partTwo :: [String] -> Int
partTwo = fst . foldl munch (0, 50)
  where
    munch (n, r) (d : s) = (n + abs n' + b, r')
      where
        op = if d == 'L' then (-) else (+)
        (n', r') = divMod (r `op` read s) 100
        -- if we rotate left we undercount by 1 unless we start at zero
        b = if (d == 'L' && r' == 0) then 1 else 0 + if (d == 'L' && r == 0) then -1 else 0
