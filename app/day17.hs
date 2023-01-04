module Main where


import           AoC                 (applyInput)
import           Control.Applicative (liftA3)
import           Data.Bifunctor      (first)
import           Data.Function       ((&))
import           Data.Hashable       (Hashable)
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as Set
import           Data.List           (foldl', unfoldr)
import           Data.Tuple.Extra    (fst3, snd3, thd3)
import           Text.Parsec         ((<|>), char, many1, newline, sepEndBy)
import           Text.Parsec.String  (Parser)


type Coord2 = (Int, Int)
type Coord3 = (Int, Int, Int)
type Coord4 = (Int, Int, Int, Int)


trimap :: (a -> d) -> (b -> e) -> (c -> f) -> (a, b, c) -> (d, e, f)
trimap f g h (x, y, z) = (f x, g y, h z)


fst4 :: (a, b, c, d) -> a
fst4 (x, _, _, _) = x

snd4 :: (a, b, c, d) -> b
snd4 (_, y, _, _) = y

thd4 :: (a, b, c, d) -> c
thd4 (_, _, z, _) = z

fth4 :: (a, b, c, d) -> d
fth4 (_, _, _, a) = a


fourmap :: (a -> e) -> (b -> f) -> (c -> g) -> (d -> h) -> (a, b, c, d) -> (e, f, g, h)
fourmap f g h i (x, y, z, a) = (f x, g y, h z, i a)


liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f x y z a = f <$> x <*> y <*> z <*> a


countNeighbours :: Hashable a => (a -> [a]) -> HashSet a -> a -> Int
countNeighbours neighF cube c =
    length $ filter (`Set.member` cube) allNeigh
  where
    allNeigh = neighF c


checkNeighbours :: Hashable a => (a -> [a]) -> HashSet a -> HashSet a -> a -> HashSet a
checkNeighbours neighF cube nextCube c
    | neighbours == 3                      = Set.insert c nextCube
    | Set.member c cube && neighbours == 2 = Set.insert c nextCube
    | otherwise                            = nextCube
  where
    neighbours = countNeighbours neighF cube c


next :: Hashable a => (a -> [a]) -> (HashSet a -> [a]) -> HashSet a -> HashSet a
next neigh allCoords cube =
    foldl' (checkNeighbours neigh cube) Set.empty (allCoords cube)


solveP2 :: HashSet Coord2 -> Int
solveP2 cube0 = Set.size $ cubes !! 6
  where
    cube4 = Set.map (\(x, y) -> (x, y, 0, 0)) cube0
    nextF = next neighF allCoords

    cubes = cube4 : unfoldr (\c -> let nextc = nextF c in Just (nextc, nextc)) cube4
    allCoords cube =
        let minx = minimum (Set.map fst4 cube)
            miny = minimum (Set.map snd4 cube)
            minz = minimum (Set.map thd4 cube)
            mina = minimum (Set.map fth4 cube)
            maxx = maximum (Set.map fst4 cube)
            maxy = maximum (Set.map snd4 cube)
            maxz = maximum (Set.map thd4 cube)
            maxa = maximum (Set.map fth4 cube)
        in liftA4 (,,,) [minx - 1 .. maxx + 1]
                        [miny - 1 .. maxy + 1]
                        [minz - 1 .. maxz + 1]
                        [mina - 1 .. maxa + 1]
    neighF c = let axisFun  = [(+1), id, subtract 1]
               in filter (/= c) $ liftA4 fourmap axisFun axisFun axisFun axisFun <*> [c]


solveP1 :: HashSet Coord2 -> Int
solveP1 cube0 = Set.size $ cubes !! 6
  where
    cube3 = Set.map (\(x, y) -> (x, y, 0)) cube0
    nextF = next neighF allCoords

    cubes = cube3 : unfoldr (\c -> let nextc = nextF c in Just (nextc, nextc)) cube3
    allCoords cube =
        let minx = minimum (Set.map fst3 cube)
            miny = minimum (Set.map snd3 cube)
            minz = minimum (Set.map thd3 cube)
            maxx = maximum (Set.map fst3 cube)
            maxy = maximum (Set.map snd3 cube)
            maxz = maximum (Set.map thd3 cube)
        in liftA3 (,,) [minx - 1 .. maxx + 1]
                       [miny - 1 .. maxy + 1]
                       [minz - 1 .. maxz + 1]
    neighF c = let axisFun  = [(+1), id, subtract 1]
               in filter (/= c) $ liftA3 trimap axisFun axisFun axisFun <*> [c]


cubesP :: Parser (HashSet Coord2)
cubesP = do
    rows <- rowP `sepEndBy` newline
    let numberedRows = zipWith (\i -> map (first (i, ))) [0 ..] rows
        onCubes      = numberedRows
                     & concat
                     & filter snd
                     & map fst
    return $ Set.fromList onCubes
  where
    rowP = zip [0 ..] <$> many1 cubeP
    cubeP = (char '#' >> return True) <|> (char '.' >> return False)



main :: IO ()
main = applyInput cubesP solveP1 solveP2