module Main where


import AoC                (applyInput, commaSepP, intP)
import Data.List          (minimumBy)
import Data.Maybe         (catMaybes)
import Text.Parsec        ((<|>), char, newline, sepBy)
import Text.Parsec.String (Parser)


nextTStamp :: (Int, Int) -> (Int, Int) -> (Int, Int)
nextTStamp (curX, ns) (off, ni) = (findx2 curX, ns * ni)
  where
    findx2 x
        | x `mod` ni == (-off) `mod` ni = x
        | otherwise                     = findx2 (x + ns)


solveP2 :: (Int, [Maybe Int]) -> Int
solveP2 (_, busses) = fst $ foldl1 nextTStamp offsets
  where
    offsets = catMaybes $ zipWith ((<*>).Just.(,)) [0..] busses


solveP1 :: (Int, [Maybe Int]) -> Int
solveP1 (time, busses) = uncurry (*) $ minimumBy compareWait $ zip bussesWithId waitTimes
  where
    waitTimes         = map (\bus -> (bus - (time `mod` bus)) `mod` bus) bussesWithId
    compareWait b1 b2 = compare (snd b1) (snd b2)
    bussesWithId      = catMaybes busses


bussesP :: Parser (Int, [Maybe Int])
bussesP = do
    time   <- intP
    _      <- newline
    busses <- ((Just <$> intP) <|> (char 'x' >> return Nothing)) `sepBy` commaSepP
    return (time, busses)


main :: IO ()
main = applyInput bussesP solveP1 solveP2