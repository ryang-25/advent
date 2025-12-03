module Main (main) where

import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as TIO

import qualified Day1 (partOne, partTwo)
import qualified Day2 (partOne, partTwo)
import qualified Day3 (partOne, partTwo)
import Utils

data S = forall a. (Show a) => S a

instance Show S where
  show (S a) = show a

pureS :: (Show b, Show c) => (a -> b) -> (a -> c) -> a -> IO S
pureS f g = pure . S . (&&&) f g

readLines :: FilePath -> IO [T.Text]
readLines = fmap T.lines . TIO.readFile

run :: Int -> IO S
run 1 = readLines "data/day1.txt" >>= pureS Day1.partOne Day1.partTwo
run 2 = readLines "data/day2.txt" >>= pureS Day2.partOne Day2.partTwo
run 3 = readLines "data/day3.txt" >>= pureS Day3.partOne Day3.partTwo
run _ = pure $ S "this challenge is not supported"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> TIO.putStrLn "advent <challenge>"
    (x : _) -> run (read x) >>= print
