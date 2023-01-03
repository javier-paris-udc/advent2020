module Main where


import           AoC                 (applyInput)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Text.Parsec.String  (Parser)
import           Text.Parsec         ((<|>), char, many1, newline, sepEndBy)
import           Data.Bifunctor      (bimap, first)
import           Control.Applicative (liftA2)
import           Data.Function       ((&))


type Coord   = (Int, Int)
type SeatMap = HashMap Coord Bool


visible :: Int -> Int -> SeatMap -> Coord -> Int
visible maxX maxY seats coord = length $ filter id $ map (exploreDir coord) dirFuns
  where
    axisFuns   = [(+1), id, subtract 1]
    dirFuns    = liftA2 bimap [(+1), subtract 1] axisFuns
              ++ liftA2 bimap [id] [(+1), subtract 1]
    out c      = fst c > maxX || snd c > maxY || fst c < 0 || snd c < 0

    exploreDir c f
        | out c     = False
        | otherwise = Map.findWithDefault (exploreDir next f) next seats
      where
        next = f c


neighbours :: SeatMap -> Coord -> Int
neighbours seats coord = length $ filter isUsed allCoords
  where
    axisFuns  = [(+1), id, subtract 1]
    coordFuns = liftA2 bimap axisFuns axisFuns
    allCoords = map ($ coord) coordFuns
    isUsed c  = c /= coord && Map.findWithDefault False c seats


runRound :: (SeatMap -> Coord -> Int) -> Int -> SeatMap -> SeatMap
runRound neighFun neighLimit s = Map.mapWithKey checkSeat s
  where
    checkSeat coord used
        | used      = neighFun s coord < neighLimit
        | otherwise = neighFun s coord == 0


rounds :: (SeatMap -> Coord -> Int) -> Int -> SeatMap -> SeatMap
rounds neighFun neighLimit s
    | next == s = s
    | otherwise = rounds neighFun neighLimit next
  where
    next = runRound neighFun neighLimit s


solveP2 :: SeatMap -> Int
solveP2 seats = seats
              & rounds (visible maxX maxY) 5
              & Map.filter id
              & Map.size
  where
    maxX = maximum (map fst (Map.keys seats))
    maxY = maximum (map snd (Map.keys seats))


solveP1 :: SeatMap -> Int
solveP1 = Map.size . Map.filter id . rounds neighbours 4


seatsP :: Parser SeatMap
seatsP = do
    rows <- rowP `sepEndBy` newline
    let numberedRows = zipWith (map . first . (,)) [0 ..] rows
        seats        = filter snd (concat numberedRows)
    return $ Map.fromList seats
  where
    rowP = zip [0 ..] <$> many1 ((char '.' >> return False)
                              <|>(char 'L' >> return True))


main :: IO ()
main = applyInput seatsP solveP1 solveP2