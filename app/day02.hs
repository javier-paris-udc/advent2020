module Main where


import Text.Parsec.String (Parser)
import Text.Parsec        (char, letter, many1, newline, sepEndBy1, space, string)
import AoC                (applyInput, intP)


data Password =
    Password { low :: Int
             , high :: Int
             , lett :: Char
             , pass :: String
             }
        deriving (Show, Eq)


isValidP1 :: Password -> Bool
isValidP1 p =
    let freq = length $ filter (== lett p) (pass p)
    in low p <= freq && freq <= high p


solveP1 :: [Password] -> Int
solveP1 = length.filter isValidP1


isValidP2 :: Password -> Bool
isValidP2 p =
    (passwd !! (high p - 1) == c) `xor` (passwd !! (low p -1) == c)
  where
    c      = lett p
    passwd = pass p
    xor    = (/=)


solveP2 :: [Password] -> Int
solveP2 = length.filter isValidP2


passwordP :: Parser Password
passwordP =
    do
        n1  <- intP
        _   <- char '-'
        n2  <- intP
        _   <- space
        c   <- letter
        _   <- string ": "
        str <- many1 letter
        return Password { low = n1, high = n2, lett = c, pass = str }


passwordsP :: Parser [Password]
passwordsP = passwordP `sepEndBy1` newline


main :: IO ()
main = applyInput passwordsP solveP1 solveP2