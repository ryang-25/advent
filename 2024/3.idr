module Main

import Data.Maybe
import Data.List
import Data.List1
import Data.String
import System.File

(&&&) : (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) f g x = (f x, g x)
infixr 3 &&&

liftA2 : Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x = (<*>) (map f x)

partOne : String -> Nat
partOne s = _
  where
    close (c::cs) s =
      case c of
        ')' => (cs, reverse s)
        _ => close cs (c :: cs)

    ident (c::cs) s =
      let s' = c :: s in
      case c of
        '(' => (cs, reverse s')
        _ => ident cs s'

    validate : String -> Maybe Nat
    validate s =
      case map parsePositive ns of
        (a:::b::_) => liftA2 (mult) a b
        _ => Nothing

    parse (c::cs) =
      case c of
        'm' => let (cs', m) = ident (c::cs) in


-- partOne s = parse (unpack s) 0
--   where
--     advance : List Char -> (List Char, List Char)
--     advance (c::cs) =
--       case c of
--         ')' => (cs, [c])
--         _   => let (cs', expr) = advance cs in (cs', c :: expr)
--     advance [] = ([], [])

    validate : String -> Maybe Nat
    validate s =
      case map parsePositive ns of
        (a:::b::_) => liftA2 (mult) a b
        _ => Nothing
--     where
--       len = length s
--       ns = split (==',') $ substr (length "mul(") (minus len 5) s

--     parse : List Char -> Nat -> Nat
--     parse [] n = n
--     parse cs n =
--       let (cs', e) = advance cs in
--       parse cs' $ plus n $ fromMaybe 0 $ validate $ pack e



-- partOne : List (List1 Nat) -> Nat
-- partOne = length . filter (==True) . map (allValid . forget)

-- partTwo : List (List1 Nat) -> Nat
-- partTwo = length . filter (==True) . map removeOneValid

-- fileInput : List String -> List (List1 Nat)
-- fileInput = map $ map stringToNatOrZ . split (==' ')

main : IO ()
main = do
  file <- readFile "data/3.txt"
  case file of
    Left err => printLn err
    Right content => printLn $ partOne content
      -- let content' =  fileInput $ lines $ content in
      -- printLn $ partOne &&& partTwo $ content'
