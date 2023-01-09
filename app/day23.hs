{-# LANGUAGE RecordWildCards #-}
module Main where


import AoC (applyInput)
import Text.Parsec.String (Parser)
import Text.Parsec (many1, digit)
import Data.List (singleton)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq (..), index, deleteAt)
import Control.Monad.State.Strict (State, get, put, execState)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Control.Monad (replicateM_)
import Data.Function ((&))
import Data.Foldable (toList)
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import Data.Bifunctor (first)


data CupSt = CupSt {cups    :: Seq Int,
                    current :: Int,
                    maxCup  :: Int,
                    minCup  :: Int,
                    idx     :: Seq IntSet}
                   deriving (Show, Eq)


next :: Seq Int -> Int -> Int
next cups n = (n + 1) `mod` Seq.length cups


nextIdx :: Int -> Seq Int -> Int
nextIdx n cups = fromJust $ Seq.findIndexL (== next cups n) cups


createIdx :: Seq Int -> Seq IntSet
createIdx = fmap (Set.fromList . toList) . Seq.chunksOf 10000

removeIdx :: Int -> Int -> Seq IntSet -> Seq IntSet
removeIdx x xIx idx =
    case idx of
        Empty                   -> Empty
        s :<| sets
            | Set.size s > xIx
           && Set.size s > 5000 -> Set.delete x s :<| sets
            | Set.size s > xIx  -> case sets of
                                    s2 :<| s2Sets -> {-# SCC "fuseSets" #-} Set.delete x s `Set.union` s2 :<| s2Sets
                                    Empty         -> Set.delete x s :<| Empty
            | otherwise         -> s :<| removeIdx x (xIx - Set.size s) sets


splitSet :: Int -> Seq Int -> Int -> Seq IntSet
splitSet start cup elements =
    fmap (Set.fromList . toList) seqs
  where
    seqs  = Seq.chunksOf 10000 $ Seq.take elements $ Seq.drop start cup


storeIdx :: Seq Int -> [Int] -> Int -> Seq IntSet -> Seq IntSet
storeIdx cup xs xsIx idx = aux 0 xs xsIx idx
  where
   aux n xs xsIx idx =
    case idx of
        Empty                   -> Seq.singleton $ Set.fromList xs
        s :<| sets
            | Set.size s > xsIx -> let newS = foldr Set.insert s xs
                                   in if Set.size newS > 15000
                                      then splitSet n cup (Set.size newS) Seq.>< sets
                                      else newS:<| sets
            | otherwise         -> s :<| aux (n+Set.size s) xs (xsIx - Set.size s) sets


pick :: Int -> State CupSt [Int]
pick n
    | n == 0    = return []
    | otherwise = do
        CupSt { .. } <- get
        let nextIx = next cups current
            x = cups `index` nextIx
        if nextIx > current
            then put $ CupSt { cups    = deleteAt nextIx cups
                             , idx     = removeIdx x nextIx idx
                             , ..}
            else put $ CupSt { cups    = deleteAt nextIx cups
                             , current = current - 1
                             , idx     = removeIdx x nextIx idx
                             , ..}
        rest <- pick (n-1)
        return (x : rest)


getNIdx :: Int -> State CupSt Int
getNIdx n = do
    CupSt { .. } <- get
    let start = getStartIndex n 0 idx
    let ix = fromJust $ Seq.findIndexL (== n) (Seq.drop start cups)
    return $ start + ix


getStartIndex n i idx =
        case idx of
            Empty                  -> undefined
            s :<| sets
                | n `Set.member` s -> i
                | otherwise        -> getStartIndex n (i + Set.size s) sets


store :: [Int] -> State CupSt ()
store cps = do
    CupSt { .. } <- get
    let nextN    =  adjustN cps minCup maxCup (cups `index` current - 1)
    nextNIdx     <- getNIdx nextN
    let newCups = foldr (Seq.insertAt (nextNIdx + 1)) cups cps
        newIdx  = storeIdx newCups cps (nextNIdx + 1) idx
    if nextNIdx < current then put CupSt {cups    = newCups
                                         ,current = current + 3
                                         ,idx     = newIdx
                                         ,..}
                          else put CupSt {cups = newCups
                                         ,idx  = newIdx
                                         ,..}
  where
    adjustN l minCup maxCup n
        | n < minCup                = adjustN l minCup maxCup maxCup
        | n `elem` l && n == minCup = adjustN l minCup maxCup maxCup
        | n `elem` l                = adjustN l minCup maxCup (n - 1)
        | otherwise                 = n


advanceCurrent :: State CupSt ()
advanceCurrent = do
    CupSt { .. } <- get
    put CupSt {current = (current + 1) `mod` Seq.length cups, .. }


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


solveP2 :: Seq Int -> Int
solveP2 cupSeq = state0
               & execState (replicateM_ 1_000_000 move)
               & cups
               & seqFrom 1
               & drop 1
               & take 2
               & product
  where
    allCups = Seq.fromList $ toList cupSeq ++ [10..1_000_000]
    state0  = CupSt {cups    = allCups
                    ,current = 0
                    ,maxCup  = 1_000_000
                    ,minCup  = 1
                    ,idx     = createIdx allCups}


solveP1 :: Seq Int -> [Int]
solveP1 cupSeq = state0
               & execState (replicateM_ 100 move)
               & cups
               & seqFrom 1
               & drop 1
  where
    state0 = CupSt {cups    = cupSeq
                   ,current = 0
                   ,maxCup  = 9
                   ,minCup  = 1
                   ,idx     = createIdx cupSeq}


cupP :: Parser (Seq Int)
cupP = Seq.fromList . map (read . singleton) <$> many1 digit


main :: IO ()
main = applyInput cupP solveP1 solveP2