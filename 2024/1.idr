module Main

import Data.List
import Data.Maybe
import Data.Nat
import Data.SortedMap
import Data.String
import Data.Zippable
import System.File

(&&&) : (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) f g x = (f x, g x)
infixr 3 &&&

absDiff : Nat -> Nat -> Nat
absDiff a b = minus (maximum a b) $ minimum a b

partOne : (List Nat, List Nat) -> Nat
partOne = sum
  . uncurry (zipWith absDiff)
  . mapHom sort

partTwo : (List Nat, List Nat) -> Nat
partTwo (ks, vs) = foldl (\k => plus . mult k $ fromMaybe 0 $ lookup k sm) 0 ks
  where
    sm = foldr (update (pure . fromMaybe 1 . map (plus 1))) empty vs

fileInput : List String -> (List Nat, List Nat)
fileInput = mapHom (map (stringToNatOrZ . ltrim))
  . unzip
  . map (break (==' '))

main : IO ()
main = do
  file <- readFile "data/1.txt"
  case file of
    Left err => printLn err
    Right content =>
      let content' = fileInput $ lines $ content in
        printLn $ partOne &&& partTwo $ content'
