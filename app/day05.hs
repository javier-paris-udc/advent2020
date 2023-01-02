module Main where


import AoC                (applyInput)
import Text.Parsec.String (Parser)
import Text.Parsec        ((<|>), char, count, newline, sepEndBy1)
import Data.List          (foldl', sort)
import Data.Function      ((&))


data BoardPass = BoardPass { row :: Int, seat :: Int } deriving (Show, Eq, Ord)


instance Enum BoardPass where
    toEnum n = BoardPass {row = n `div` 8, seat = n `mod` 8}
    fromEnum = seatId


findHole :: [BoardPass] -> BoardPass
findHole passes =
    case passes of
        (b1 : b2 : bs)
            | b2 == succ b1 -> findHole (b2 : bs)
            | otherwise     -> succ b1
        _                   -> undefined


seatId :: BoardPass -> Int
seatId pass = row pass * 8 + seat pass


solveP2 :: [BoardPass] -> Int
solveP2 passes =
      passes
    & filter ((/=0) . row)
    & filter ((/=maxRow) . row)
    & sort
    & findHole
    & seatId
  where
    maxRow = maximum $ map row passes


solveP1 :: [BoardPass] -> Int
solveP1 = maximum  . map seatId


boardPassesP :: Parser [BoardPass]
boardPassesP = boardPassP `sepEndBy1` newline
  where
    boardPassP = do
        rowN  <- bin2Int <$> count 7 fb
        seatN <-  bin2Int <$> count 3 lr
        return $ BoardPass {row = rowN, seat = seatN}
    fb      = (char 'F' >> return 0) <|> (char 'B' >> return 1)
    lr      = (char 'L' >> return 0) <|> (char 'R' >> return 1)
    bin2Int = foldl' ((+) . (*2)) 0


main :: IO ()
main = applyInput boardPassesP solveP1 solveP2