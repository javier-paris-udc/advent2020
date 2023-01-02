module Main where


import AoC (applyInput)
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Text.Parsec.String (Parser)
import Text.Parsec (sepEndBy, newline, many1, lower)
import Data.List (foldl1')


type CustomsGroup = [HashSet Char]


solveP2 :: [CustomsGroup] -> Int
solveP2 =  sum . map (Set.size . foldl1' Set.intersection)


solveP1 :: [CustomsGroup] -> Int
solveP1 = sum . map (Set.size . Set.unions)


customsP :: Parser [CustomsGroup]
customsP = groupP `sepEndBy` newline
  where
    groupP   = answersP `sepEndBy` newline
    answersP = Set.fromList <$> many1 lower



main :: IO ()
main = applyInput customsP solveP1 solveP2