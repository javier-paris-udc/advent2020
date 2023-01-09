module Main where


import AoC                (applyInput1, intP)
import Data.List          (elemIndex)
import Data.Maybe         (fromJust)
import Text.Parsec        (newline)
import Text.Parsec.String (Parser)


applyN :: Int -> (a -> a) -> a -> a
applyN n f x
    | n == 0    = x
    | otherwise = applyN (n - 1) f (f x)


transform :: Int -> Int -> Int
transform subject value = (value * subject) `mod` 20201227


solve :: (Int, Int) -> Int
solve (public1, public2) =
    applyN loop1 (transform public2) 1
  where
    loop1 = fromJust $ elemIndex public1 $ iterate (transform 7) 1


publicsP :: Parser (Int, Int)
publicsP = do
    n1 <- intP
    _  <- newline
    n2 <- intP
    return (n1, n2)


main :: IO ()
main = applyInput1 publicsP solve