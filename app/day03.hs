module Main where


import AoC                (applyInput)
import Data.Array         (Array, (!), bounds, listArray)
import Text.Parsec        ((<|>), char, many1, sepEndBy, space)
import Text.Parsec.String (Parser)


type Coord = (Int, Int)
type Grid = Array Coord Bool


solve :: Grid -> Int -> Int -> Int
solve arr right down =
    length (filter id ((arr !) <$> coords))
  where
    (rows, cols) = snd $ bounds arr
    coords       = zip [0, down .. rows] (fmap (`rem` (cols + 1)) [0, right ..])


solveP2 :: Grid -> Int
solveP2 arr = product (uncurry (solve arr) <$> zip [1, 3, 5, 7, 1] [1, 1, 1, 1, 2])


solveP1 :: Grid -> Int
solveP1 arr = solve arr 3 1


squareP :: Parser Bool
squareP = (char '#' >> return True) <|> (char '.' >> return False)


lineP :: Parser [Bool]
lineP = many1 squareP


gridP :: Parser Grid
gridP =
    do
        lineList <- lineP `sepEndBy` space
        let rows = length lineList
        let cols = length (head lineList)
        return $ listArray ((0, 0), (rows - 1, cols - 1)) (concat lineList)


main :: IO ()
main = applyInput gridP solveP1 solveP2