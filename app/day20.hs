module Main where


import           AoC                 (applyInput)
import           Data.Biapplicative  (biliftA2)
import           Data.Bifunctor      (bimap, first, second)
import           Control.Applicative (liftA2)
import           Control.Arrow       ((>>>))
import           Data.Bits           (testBit)
import           Data.Function       ((&))
import           Data.List           (find, foldl', intercalate, nub)
import           Data.Tuple          (swap)
import           Data.Array          (Array
                                     ,(!)
                                     ,(//)
                                     ,array
                                     ,assocs
                                     ,bounds
                                     ,ixmap
                                     ,listArray)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as Set
import           Text.Parsec         ((<|>)
                                     ,between
                                     ,char
                                     ,digit
                                     ,many1
                                     ,newline
                                     ,sepEndBy
                                     ,sepEndBy1
                                     ,string)
import           Text.Parsec.String  (Parser)
import Data.Maybe (fromMaybe)


type Coord = (Int, Int)
type Tile  = Array Coord Bool
data Dir   = U | D | L | R deriving (Show, Eq)
data Axis  = Vertical | Horizontal


opposite :: Dir -> Dir
opposite d = case d of
    U -> D
    D -> U
    L -> R
    R -> L


flipSideAxis :: Dir -> Axis
flipSideAxis d = case d of
    U -> Horizontal
    D -> Horizontal
    L -> Vertical
    R -> Vertical


move :: Dir -> Coord -> Coord
move dir = case dir of
    U -> first  (subtract 1)
    D -> first  (+1)
    L -> second (subtract 1)
    R -> second (+1)


mirrorAxis :: Dir -> Axis
mirrorAxis = oppositeAxis . flipSideAxis
  where
    oppositeAxis Vertical   = Horizontal
    oppositeAxis Horizontal = Vertical


rev :: Int -> Int -> Int
rev side x = foldl' checkBit 0 [0 .. side]
  where
    checkBit n bit
        | testBit x bit = n + 2 ^ (side - bit)
        | otherwise     = n


boolsToInt :: [Bool] -> Int
boolsToInt  = foldl' (\n b -> if b then 2 * n + 1 else 2 * n) 0


upFace, downFace, leftFace, rightFace :: Int -> Tile -> Int
upFace    side tile = boolsToInt $ map ((tile !) . (0,))     [0 .. side]
downFace  side tile = boolsToInt $ map ((tile !) . (side,))  [0 .. side]
leftFace  side tile = boolsToInt $ map ((tile !) . (, 0))    [0 .. side]
rightFace side tile = boolsToInt $ map ((tile !) . (, side)) [0 .. side]


getFace :: Dir -> Tile -> Int
getFace dir tile =
    case dir of
        U -> upFace side tile
        D -> downFace side tile
        L -> leftFace side tile
        R -> rightFace side tile
  where
    side = fst $ snd $ bounds tile


borders :: Tile -> HashSet Int
borders tile =
    Set.fromList (faces ++ map (rev side) faces)
  where
    side  = fst $ snd $ bounds tile
    faces = [upFace    side tile
            ,downFace  side tile
            ,leftFace  side tile
            ,rightFace side tile]


findMatchingFaces :: Tile -> Tile -> (Dir, Dir)
findMatchingFaces ref tile =
      allFaces ref
    & map (\(dir, sig) -> (dir, filter (matches sig . snd) tileFaces))
    & filter (not . null . snd)
    & head
    & second (fst . head)
  where
    side             = fst $ snd $ bounds ref
    allFaces t       = [(U, upFace    side t)
                       ,(D, downFace  side t)
                       ,(L, leftFace  side t)
                       ,(R, rightFace side t)]
    tileFaces         = allFaces tile
    matches sig1 sig2 = sig1 == sig2 || sig1 == rev side sig2


mirror :: Axis -> Tile -> Tile
mirror axis tile =
    ixmap (bounds tile) (mapIx axis) tile
  where
    (sidex, sidey)          = snd $ bounds tile
    mapIx Horizontal (x, y) = (sidex - x, y)
    mapIx Vertical   (x, y) = (x, sidey - y)


rotateRight :: Tile -> Tile
rotateRight tile =
    ixmap (bimap swap swap $ bounds tile) mapIx tile
  where
    (sidex, _)   = snd $ bounds tile
    mapIx (x, y) = (sidex - y, x)


rotateLeft :: Tile -> Tile
rotateLeft tile =
    ixmap (bimap swap swap $ bounds tile) mapIx tile
  where
    (_, sidey)   = snd $ bounds tile
    mapIx (x, y) = (y, sidey - x)


rotateDirL :: Dir -> Dir
rotateDirL dir =
    case dir of
        U -> L
        L -> D
        D -> R
        R -> U

rotateDirR :: Dir -> Dir
rotateDirR dir =
    case dir of
        U -> R
        R -> D
        D -> L
        L -> U


rotateTile :: Dir -> Dir -> Tile -> Tile -> Tile
rotateTile refF tileF ref tile
    | refF == opposite tileF   =
        checkFlip
    | refF == tileF            =
        rotateTile refF (opposite tileF)   ref (mirror (flipSideAxis tileF) tile)
    | tileF == R || tileF == L =
        rotateTile refF (rotateDirL tileF) ref (rotateLeft tile)
    | otherwise                =
        rotateTile refF (rotateDirR tileF) ref (rotateRight tile)
  where
    checkFlip
        | getFace refF ref == getFace tileF tile = tile
        | otherwise  = mirror (mirrorAxis tileF) tile


rotAndFlip :: Tile -> Tile -> (Dir, Tile)
rotAndFlip ref tile = (fref, rotatedTile)
  where
    (fref, ftile) = findMatchingFaces ref tile
    rotatedTile   = rotateTile fref ftile ref tile


buildTileMap :: HashMap Int Tile -> HashMap Int (HashSet Int) -> HashMap Coord Tile
buildTileMap tiles neighbours =
    let (tile0id, tile0) = head $ Map.toList tiles
    in buildMap [(tile0id, (0,0))]
                (Map.singleton (0, 0) tile0)
                (Map.singleton tile0id (0, 0))
  where
    buildMap pending tileMap visitedTiles =
        case pending of
            []        -> tileMap
            (i, c):is ->
                let tile      = tileMap Map.! (visitedTiles Map.! i)
                    allNeighs = neighbours Map.! i
                    newNeighs = Set.filter (not . (`Map.member` visitedTiles)) allNeighs
                    neighList = Set.toList newNeighs
                    oriented  = rotAndFlip tile . (tiles Map.!) <$> neighList
                    coords    = zip neighList (map ((`move` c). fst) oriented)
                in buildMap (nub $ coords ++ is)
                            (foldl' (addTile c) tileMap oriented)
                            (insertAll coords visitedTiles)
    addTile c t (dir, tile)  = Map.insert (move dir c) tile t
    insertAll coords visited = foldl' (\m (k,v) -> Map.insert k v m)visited coords


findNeighbours :: HashMap Int (HashSet Int) -> HashMap Int (HashSet Int)
findNeighbours tiles = Map.mapWithKey findMatches tiles
  where
    findMatches i sides = tiles
                        & Map.filter (any (`elem` sides))
                        & Map.keysSet
                        & Set.delete i


tileMapBounds :: HashMap Coord Tile -> (Coord, Coord)
tileMapBounds tiles =
    let coords = Map.keys tiles
        minx = minimum (map fst coords)
        maxx = maximum (map fst coords)
        miny = minimum (map snd coords)
        maxy = maximum (map snd coords)
    in ((minx, miny), (maxx, maxy))


showTile :: Tile -> String
showTile tile = intercalate "\n" $ map row [0 .. sizex]
  where
    (sizex, sizey) = snd (bounds tile)
    row i = map (\j -> if tile ! (i,j) then '#' else '.') [0 .. sizey]


removeGaps :: Tile -> Tile
removeGaps tile = tile
                & assocs
                & filter (inside . fst)
                & map (first $ bimap (subtract 1) (subtract 1))
                & array ((0, 0), (sizex - 2, sizey - 2))
  where
    (sizex, sizey) = snd (bounds tile)
    inside (x, y)  = x > 0 && x < sizex && y > 0 && y < sizey


fuse :: HashMap Coord Tile -> Tile
fuse tiles = listArray ((0, 0), (sizex - 1, sizey - 1))
           $ concatMap row [0 .. ((maxx - minx + 1) * side)]
  where
    ((minx, miny), (maxx, maxy)) = tileMapBounds tiles
    side    = 1 + fst (snd $ bounds (tiles Map.! (minx, miny)))
    sizex   = (maxx - minx + 1) * side
    sizey   = (maxy - miny + 1) * side
    row i   = map (get i) [0 .. (maxy - miny + 1) * side - 1]
    get i j = let c = (i `div` side + minx, j `div` side + miny)
              in (tiles Map.! c) ! (i `mod` side, j `mod` side)


checkMonster :: Tile -> Tile -> Coord -> Tile
checkMonster tile monster c
    | all (\(coord, v) -> not v || tile ! shift c coord) (assocs monster)
                = tile // map (\(coord, _) -> (shift c coord, False))
                              (filter snd (assocs monster))
    | otherwise = tile
  where
    shift      = biliftA2 (+) (+)


searchMonster :: Tile -> Tile -> Tile
searchMonster tile monster = tileWithoutMonster
  where
    (monsterx, monstery) = snd $ bounds monster
    (tilex,    tiley)    = snd $ bounds tile
    positionsx           = [0 .. tilex - monsterx]
    positionsy           = [0 .. tiley - monstery]
    positions            = liftA2 (,) positionsx positionsy
    tileWithoutMonster   = foldl' (`checkMonster` monster) tile positions


findMonster :: Tile -> Int
findMonster tile = fromMaybe undefined (find (< tilePixels) $
                       map tileOn tilesWithoutMonster)
  where
    tileOn     = foldl' (\s v -> if v then s + 1 else s) 0
    tilePixels = tileOn tile
    monster    = listArray ((0,0), (2,19)) $ map (== '#') "                  # \
                                                          \#    ##    ##    ###\
                                                          \ #  #  #  #  #  #   "
    monsters   = let allRotations = take 4 $ iterate rotateRight monster
                 in allRotations ++ map (mirror Vertical) allRotations
    tilesWithoutMonster = map (searchMonster tile) monsters


solveP2 :: HashMap Int Tile -> Int
solveP2 tiles = findMonster fused
  where
    signatures = Map.map borders tiles
    neighbours = findNeighbours signatures
    tileMap    = buildTileMap tiles neighbours
    ungapped   = Map.map removeGaps tileMap
    fused      = fuse ungapped


solveP1 :: HashMap Int Tile -> Int
solveP1 = Map.map borders
      >>> findNeighbours
      >>> Map.filter ((==2) . length)
      >>> Map.keysSet
      >>> product


jigSawP :: Parser (HashMap Int Tile)
jigSawP = do
    tiles <- tileP `sepEndBy1` newline
    return $ Map.fromList tiles
  where
    tileP = do
        num  <- between (string "Tile ") (char ':' >> newline) intP
        rows <- rowP `sepEndBy` newline
        let side = length rows
        return (num, listArray ((0,0), (side - 1, side - 1)) (concat rows))

    rowP = do
        many1 $ (char '.' >> return False) <|> (char '#' >> return True)

    intP = read <$> many1 digit


main :: IO ()
main = applyInput jigSawP solveP1 solveP2