module Utils ((&&&), readT) where

import qualified Data.Text as T

infixr 3 &&&

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) f g x = (f x, g x)

readT :: Read a => T.Text -> a
readT = read . T.unpack
