{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Main where


import           AoC                        (applyInput)
import           Control.Monad              (replicateM_)
import           Control.Monad.State.Strict (State, get, put, execState)
import           Data.Function              ((&))
import           Data.HashMap.Strict        (HashMap, (!))
import qualified Data.HashMap.Strict        as Map
import           Data.List                  (singleton, unfoldr)
import           Text.Parsec                (many1, digit)
import           Text.Parsec.String         (Parser)


type Cups = HashMap Int Int
data CupSt = CupSt {cups    :: Cups,
                    current :: Int,
                    maxCup  :: Int,
                    minCup  :: Int}
                   deriving (Show, Eq)



next :: Int -> Int -> Int
next size n = (n + 1) `mod` size


createMap :: [Int] -> HashMap Int Int
createMap ns =
    case ns of
        []     -> Map.empty
        (x:xs) -> Map.fromList $ aux x (x:xs)
  where
    aux x0 l = case l of
        []             -> undefined
        [x1]           -> [(x1, x0)]
        (x1 : x2 : xs) -> (x1, x2) : aux x0 (x2 : xs)


pick :: Int -> State CupSt [Int]
pick n
    | n == 0    = return []
    | otherwise = do
        CupSt { .. } <- get
        let nxt     = cups ! current
            nxtNext = cups ! nxt
            newCups  = Map.delete nxt (Map.insert current nxtNext cups)
        put $ CupSt { cups = newCups, ..}
        rest <- pick (n-1)
        return (nxt : rest)


insertL :: Cups -> Int -> [Int] -> Cups
insertL cupMap pos cps =
    aux cupMap pos cps (cupMap ! pos)
  where
    aux cmap prev l lastNext =
        case l of
            []     -> Map.insert prev lastNext cmap
            (x:xs) -> aux (Map.insert prev x cmap) x xs lastNext


store :: [Int] -> State CupSt ()
store cps = do
    CupSt { .. } <- get
    let nextN   = adjustN cps minCup maxCup (current - 1)
        newCups = insertL cups nextN cps
    put CupSt {cups = newCups, ..}
  where
    adjustN l minCup maxCup n
        | n < minCup                = adjustN l minCup maxCup maxCup
        | n `elem` l && n == minCup = adjustN l minCup maxCup maxCup
        | n `elem` l                = adjustN l minCup maxCup (n - 1)
        | otherwise                 = n


advanceCurrent :: State CupSt ()
advanceCurrent = do
    CupSt { .. } <- get
    put CupSt {current = cups ! current, .. }


move :: State CupSt ()
move = do
    next3 <- pick 3
    store next3
    advanceCurrent


seqFrom :: Int -> Cups -> [Int]
seqFrom n cupMap = n : unfoldr (\x -> if x == n then Nothing
                                      else Just (x, cupMap ! x))
                               (cupMap ! n)


solveP2 :: [Int] -> Int
solveP2 cupSeq = state0
               & execState (replicateM_ 10_000_000 move)
               & cups
               & seqFrom 1
               & drop 1
               & take 2
               & product
  where
    state0  = CupSt {cups    = createMap $ cupSeq ++ [10..1_000_000]
                    ,current = head cupSeq
                    ,maxCup  = 1_000_000
                    ,minCup  = 1}


solveP1 :: [Int] -> [Int]
solveP1 cupSeq = state0
               & execState (replicateM_ 100 move)
               & cups
               & seqFrom 1
               & drop 1
  where
    state0 = CupSt {cups    = createMap cupSeq
                   ,current = head cupSeq
                   ,maxCup  = 9
                   ,minCup  = 1}


cupP :: Parser [Int]
cupP = map (read . singleton) <$> many1 digit


main :: IO ()
main = applyInput cupP solveP1 solveP2