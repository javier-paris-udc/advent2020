module Main where


import AoC                 (applyInput, intP)
import Control.Applicative (liftA2, liftA3)
import Data.Foldable       (find)
import Text.Parsec         (sepEndBy1, spaces)
import Text.Parsec.String  (Parser)


solveP1 :: [Int] -> Int
solveP1 l =
    case find ((==2020).uncurry (+)) (liftA2 (,) l l) of
        Just (x, y) -> x * y
        Nothing -> error "no solution"


solveP2 :: [Int] -> Int
solveP2 l =
    case find ((==2020).(\(x,y,z) -> x + y + z)) (liftA3 (,,) l l l) of
        Just (x, y, z) -> x * y * z
        Nothing -> error "no solution"


numListP :: Parser [Int]
numListP = intP `sepEndBy1` spaces


main :: IO ()
main = applyInput numListP solveP1 solveP2