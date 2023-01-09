module Main where


import           AoC                 (applyInput)
import           Control.Applicative (liftA2)
import           Control.Arrow       ((>>>))
import           Data.Bifunctor      (bimap, first, second)
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as Set
import           Data.List           (foldl')
import           Text.Parsec         (choice, many1, newline, sepEndBy1, string, try)
import           Text.Parsec.String  (Parser)



data Dir = E | W | SE | SW | NE | NW deriving (Show, Eq)
type Coord = (Int, Int)


move :: Coord -> Dir -> Coord
move c dir =
    case dir of
        E  -> second (+1) c
        W  -> second (subtract 1) c
        SE -> first (+1) c
        SW -> bimap (+1) (subtract 1) c
        NE -> bimap (subtract 1) (+1) c
        NW -> first (subtract 1) c


getTile :: [Dir] -> Coord
getTile = foldl' move (0, 0)


flipTile :: HashSet Coord -> Coord -> HashSet Coord
flipTile s c
    | c `Set.member` s = Set.delete c s
    | otherwise        = Set.insert c s


buildMap :: [Coord] -> HashSet Coord
buildMap = foldl' flipTile Set.empty


neighbours :: Coord -> [Coord]
neighbours c = map (move c) [E, W, SE, SW, NE, NW]


day :: HashSet Coord -> HashSet Coord
day s = foldl' check Set.empty allCoords
  where
    minX        = minimum (Set.map fst s)
    minY        = minimum (Set.map snd s)
    maxX        = maximum (Set.map fst s)
    maxY        = maximum (Set.map snd s)
    allCoords   = liftA2 (,) [minX - 1 .. maxX + 1] [minY - 1 .. maxY + 1]
    check set c
        | blackNeighs == 2  = Set.insert c set
        | c `Set.member` s
        && blackNeighs == 1 = Set.insert c set
        | otherwise         = set
      where
        blackNeighs = length $ filter (`Set.member` s) (neighbours c)


applyN :: Int -> (a -> a) -> a -> a
applyN n f x
    | n == 0    = x
    | otherwise = applyN (n - 1) f (f x)


solveP2 :: [[Dir]] -> Int
solveP2 = map getTile
      >>> buildMap
      >>> applyN 100 day
      >>> Set.size


solveP1 :: [[Dir]] -> Int
solveP1 = map getTile
      >>> buildMap
      >>> Set.size


hexP :: Parser [[Dir]]
hexP = do
    tileP `sepEndBy1` newline
  where
    tileP = many1 dirP
    dirP  = choice [try $ string "e"  >> return E
                   ,try $ string "w"  >> return W
                   ,try $ string "se" >> return SE
                   ,try $ string "sw" >> return SW
                   ,try $ string "ne" >> return NE
                   ,try $ string "nw" >> return NW
                   ]


main :: IO ()
main = applyInput hexP solveP1 solveP2