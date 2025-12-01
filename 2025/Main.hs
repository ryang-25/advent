module Main (main) where

import Control.Arrow ((&&&))
import qualified Day1 (partOne, partTwo)
import System.Environment (getArgs)

data S = forall a. (Show a) => S a

instance Show S where
  show (S a) = show a

readLines :: String -> IO [String]
readLines = fmap lines . readFile

run :: Int -> IO S
run 1 = do
  content <- readLines "data/day1.txt"
  pure $ S ((Day1.partOne &&& Day1.partTwo) content)

main :: IO ()
main = do
  args <- getArgs
  let len = length args
  case len of
    0 -> putStrLn "advent <challenge>"
    _ -> do
      let selection = read $ head args
      run selection >>= print
