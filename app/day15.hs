module Main where
import AoC (applyInput, commaSepP, intP)
import Text.Parsec.String (Parser)
import Text.Parsec (sepBy)
import qualified Data.HashMap.Strict as Map
import Data.List (unfoldr)
import Data.HashMap.Strict ((!?))


solve :: [Int] -> Int -> Int
solve nums pos = spokenNums !! pos
  where
    spokenNums = nums ++ unfoldr nextSpoken (last nums, length nums - 1, initMap)

    initMap    = let startingNums = take (length nums - 1) nums
                 in Map.fromList (zip startingNums [0..])

    nextSpoken (n, i, numMap) =
        case numMap !? n of
            Nothing -> Just (0,     (0,     i + 1, Map.insert n i numMap))
            Just p  -> Just (i - p, (i - p, i + 1, Map.insert n i numMap))


solveP2 :: [Int] -> Int
solveP2 nums = solve nums (30_000_000 - 1)


solveP1 :: [Int] -> Int
solveP1 nums = solve nums (2020 - 1)


numsP :: Parser [Int]
numsP = intP `sepBy` commaSepP


main :: IO ()
main = applyInput numsP solveP1 solveP2