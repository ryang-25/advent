module Utils ((&&&), readT, same, chunks) where

import qualified Data.Text as T

infixr 3 &&&

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) f g x = (f x, g x)

readT :: Read a => T.Text -> a
readT = read . T.unpack

same :: Eq a => [a] -> Bool
same (x:y:xs) = x == y && same (y:xs)
same _ = True

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = let (x, xs') = splitAt n xs in x : chunks n xs'