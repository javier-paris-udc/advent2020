{-# LANGUAGE RecordWildCards #-}
module Main where


import AoC (applyInput)
import Text.Parsec.String (Parser)
import Text.Parsec (many1, digit)
import Data.List (singleton, foldl')
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, index, deleteAt)
import Control.Monad.State.Strict (State, get, put, execState)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Control.Monad (replicateM_)
import Data.Function ((&))
import Data.Foldable (toList)



data CupSt = CupSt {cups    :: Seq Int,
                    current :: Int,
                    maxCup  :: Int,
                    minCup  :: Int}
                   deriving (Show, Eq)


next :: Seq Int -> Int -> Int
next cups n = (n + 1) `mod` Seq.length cups


nextIdx :: Int -> Seq Int -> Int
nextIdx n cups = fromJust $ Seq.findIndexL (== next cups n) cups


pick :: Int -> State CupSt [Int]
pick n
    | n == 0    = return []
    | otherwise = do
        CupSt { .. } <- get
        currentIx   <- getNIdx current
        let x = cups `index` next cups currentIx
        put $ CupSt { cups = deleteAt (next cups currentIx) cups, ..}
        rest <- pick (n-1)
        return (x : rest)


getNIdx :: Int -> State CupSt Int
getNIdx n = do
    CupSt { .. } <- get
    return $ fromJust $ Seq.findIndexL (== n) cups


store :: [Int] -> State CupSt ()
store cps = do
    CupSt { .. } <- get
    let nextN    =  adjustN cps minCup maxCup (current - 1)
    nextNIdx     <- getNIdx nextN
    let newCups = foldl' (\cp x -> Seq.insertAt (nextNIdx + 1) x cp) cups (reverse cps)
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
    currentIx <- getNIdx current
    put CupSt {current = cups `index` next cups currentIx, .. }


move :: State CupSt ()
move = do
    next3 <- pick 3
    store next3
    advanceCurrent


seqFrom :: Int -> Seq Int -> [Int]
seqFrom n cups =
    let nIx = fromJust $ Seq.findIndexL (== n) cups
    in [nIx .. nIx + Seq.length cups - 1]
     & map (\ix ->  cups `index` (ix `mod` Seq.length cups))


solveP2 :: Seq Int -> [Int]
solveP2 cupSeq = state0
               & execState (replicateM_ 1 move)
               & cups
               & seqFrom 1
               & take 3
  where
    state0 = CupSt {cups    = Seq.fromList $ toList cupSeq ++ [10..1_000_000]
                   ,current = cupSeq `index` 0
                   ,maxCup  = 1_000_000
                   ,minCup  = 1}


solveP1 :: Seq Int -> [Int]
solveP1 cupSeq = state0
               & execState (replicateM_ 100 move)
               & cups
               & seqFrom 1
               & drop 1
  where
    state0 = CupSt {cups    = cupSeq
                   ,current = cupSeq `index` 0
                   ,maxCup  = maximum cupSeq
                   ,minCup  = minimum cupSeq}


cupP :: Parser (Seq Int)
cupP = Seq.fromList . map (read . singleton) <$> many1 digit


main :: IO ()
main = applyInput cupP solveP1 solveP2