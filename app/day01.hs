module Main where


import Text.Parsec.String (Parser)
import Text.Parsec (digit, many1, option, char, parse, space, sepEndBy1)
import System.Environment (getArgs)
import Control.Applicative (liftA2, liftA3)
import Data.Foldable (find)


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



intP :: Parser Int
intP =
    do
        sign <- option 1 (char '-' >> return (-1))
        n    <- many1 digit
        return (sign * read n)


numListP :: Parser [Int]
numListP = intP `sepEndBy1` space


main :: IO ()
main =
    do
        args <- getArgs
        case args of
            [inputFile] ->
                do
                    input <- readFile inputFile
                    case parse numListP "" input of
                        Left err -> print err
                        Right nums ->
                            do
                                print (solveP1 nums)
                                print (solveP2 nums)
            _ -> putStrLn "Use: day1 input"