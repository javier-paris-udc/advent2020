module Main where


import AoC                (applyInput, intP)
import Data.Biapplicative (biliftA2)
import Data.Bifunctor     (bimap, first, second)
import Data.List          (foldl')
import Text.Parsec        (char, choice, newline, sepEndBy)
import Text.Parsec.String (Parser)


type Coord  = (Int, Int)
data Dir    = E | S | W | N deriving (Show, Eq, Enum)
data Action = Move Dir Int | F Int | L Int | R Int deriving (Show, Eq)


move :: Dir -> Int -> Coord -> Coord
move E len = second (+len)
move S len = first  (+len)
move W len = second (subtract len)
move N len = first  (subtract len)


rotate :: Dir -> Int -> Dir
rotate dir deg = toEnum $ (fromEnum dir + deg `div` 90) `mod` 4


rotateWaypoint :: Coord -> Int -> Coord
rotateWaypoint wayp deg = turn wayp rightTurns
  where
    rightTurns     = deg `div` 90 `mod` 4

    turn rotWayp 0 = rotWayp
    turn (x, y) n  = turn (-y, x) (n - 1)


distance :: Coord -> Coord -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)


step :: (Coord, Dir) -> Action -> (Coord, Dir)
step (c, dir) action =
    case action of
        Move d len -> (move d   len c, dir)
        F len      -> (move dir len c, dir)
        L deg      -> (c, rotate dir (-deg))
        R deg      -> (c, rotate dir deg)


stepWaypoint :: (Coord, Coord) -> Action -> (Coord, Coord)
stepWaypoint (ship, wayp) action =
    case action of
        Move d len -> (ship, move d len wayp)
        F len      -> (biliftA2 (+) (+) ship (bimap (*len) (*len) wayp), wayp)
        L deg      -> (ship, rotateWaypoint wayp deg)
        R deg      -> (ship, rotateWaypoint wayp (-deg))


solveP2 :: [Action] -> Int
solveP2 = distance (0, 0) . fst . foldl' stepWaypoint ((0,0), (-1, 10))


solveP1 :: [Action] -> Int
solveP1 = distance (0, 0) . fst . foldl' step ((0, 0), E)


actionsP :: Parser [Action]
actionsP = actionP `sepEndBy` newline
  where
    actionP = choice [char 'N' >> Move N <$> intP
                     ,char 'S' >> Move S <$> intP
                     ,char 'E' >> Move E <$> intP
                     ,char 'W' >> Move W <$> intP
                     ,char 'F' >> F      <$> intP
                     ,char 'R' >> R      <$> intP
                     ,char 'L' >> L      <$> intP
                     ]


main :: IO ()
main = applyInput actionsP solveP1 solveP2