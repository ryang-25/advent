module Main

import Data.List
import Data.List1
import Data.String
import System.File

(&&&) : (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) f g x = (f x, g x)
infixr 3 &&&

allValid : List Nat -> Bool
allValid l@(x::y::_) = (x /= y) && allValid' l
  where
    diff = if (x < y) then flip minus else minus
    allValid' : List Nat -> Bool
    allValid' (x::y::xs) = (d >= 1) && (d <= 3) && allValid' (y::xs)
      where d = diff x y
    allValid' _ = True
allValid _ = True

removeOneValid : List1 Nat -> Bool
removeOneValid xs = any allValid $ zipWith (++) (inits xs') $ tail $ tails xs'
  where xs' = forget xs

partOne : List (List1 Nat) -> Nat
partOne = length . filter (==True) . map (allValid . forget)

partTwo : List (List1 Nat) -> Nat
partTwo = length . filter (==True) . map removeOneValid

fileInput : List String -> List (List1 Nat)
fileInput = map $ map stringToNatOrZ . split (==' ')

main : IO ()
main = do
  file <- readFile "data/2.txt"
  case file of
    Left err => printLn err
    Right content =>
      let content' =  fileInput $ lines $ content in
      printLn $ partOne &&& partTwo $ content'
