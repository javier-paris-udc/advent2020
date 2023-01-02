module Main where

import AoC                (applyInput, intP)
import Control.Arrow      ((>>>))
import Data.Bifunctor     (first)
import Data.List          (group, sort, unfoldr)
import Text.Parsec        (newline, sepEndBy)
import Text.Parsec.String (Parser)


solve :: [Int] -> Int
solve l = case l of
    x1:x2:x3:xs
        | x3 <= x1 + 3 -> solve (x1:x3:xs) + solve (x2:x3:xs)
        | otherwise    -> solve (x2:xs)
    _                  -> 1


solveP2 :: [Int] -> Int
solveP2 = product . map solve . unfoldr find3
  where
    find3 (x1:x2:xs)
        | x2 <= x1 + 2 = first (x1:) <$> find3 (x2:xs)
        | otherwise    = Just ([x1], x2:xs)
    find3 [x1]         = Just ([x1], [])
    find3 []           = Nothing


solveP1 :: [Int] -> Int
solveP1 = zipWith (flip subtract) <*> tail
      >>> filter (/= 2)
      >>> sort
      >>> group
      >>> map length
      >>> product


numsP :: Parser [Int]
numsP = do
    nums <- intP `sepEndBy` newline
    let deviceJolt = maximum nums + 3

    return $ sort (0 : deviceJolt : nums)


main :: IO ()
main = applyInput numsP solveP1 solveP2