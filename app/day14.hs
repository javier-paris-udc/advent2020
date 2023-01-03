module Main where


import           AoC                 (applyInput, intP)
import           Data.Bits           (clearBit, setBit)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.List           (foldl')
import           Text.Parsec         ((<|>)
                                     ,char
                                     ,choice
                                     ,many1
                                     ,newline
                                     ,sepEndBy
                                     ,string
                                     ,try)
import           Text.Parsec.String  (Parser)


data MaskBit = X | One | Zero deriving (Show, Eq)
type Mask    = [MaskBit]
data Inst    = Write Int Int -- pos val
             | SetMask Mask
           deriving (Show, Eq)
type Program = [Inst]
type Memory  = HashMap Int Int


writeWithMask :: (Memory -> Mask -> Int -> Int -> (Memory, Mask))
              -> (Memory, Mask)
              -> Inst
              -> (Memory, Mask)
writeWithMask doWrite (mem, mask) inst =
    case inst of
        SetMask newMask -> (mem, reverse newMask)
        Write pos val   -> doWrite mem mask pos val


solve :: (Memory -> Mask -> Int -> Int -> (Memory, Mask)) -> Program -> Int
solve doWrite insts =
    sum . fst $ foldl' (writeWithMask doWrite) (Map.empty, mask0) insts
  where
    mask0 = replicate 36 X


solveP2 :: Program -> Int
solveP2 = solve doWrite
  where
    doWrite mem mask pos val =
        (foldl' (flip $ Map.alter (const valForMap)) mem allPos, mask)
      where
        valForMap
            | val == 0  = Nothing
            | otherwise = Just val
        allPos = foldl' genPositions [pos] $ zip [0 ..] mask
        genPositions positions (bit, maskbit) =
            case maskbit of
                Zero -> positions
                One  -> map (`setBit` bit) positions
                X    -> map (`setBit` bit) positions ++ map (`clearBit` bit) positions


solveP1 :: Program -> Int
solveP1 = solve doWrite
  where
    doWrite mem mask pos val = (Map.alter (const $ newValForMap val mask) pos mem, mask)

    newValForMap val mask
        | newVal == 0 = Nothing
        | otherwise   = Just newVal
      where
        newVal = applyMask mask [0 ..] val

    applyMask _ []         _ = undefined
    applyMask m (b : bits) v =
        case m of
            []              -> v
            (X:maskBits)    -> applyMask maskBits bits v
            (One:maskBits)  -> applyMask maskBits bits (v `setBit` b)
            (Zero:maskBits) -> applyMask maskBits bits (v `clearBit` b)



programP :: Parser Program
programP = (maskP <|> writeP) `sepEndBy` newline
  where
    maskP = do
        _ <- try $ string "mask = "
        SetMask <$> many1 (choice [char 'X' >> return X
                                  ,char '1' >> return One
                                  ,char '0' >> return Zero
                                  ])
    writeP = do
        _ <- try $ string "mem["
        pos <- intP
        _ <- string "] = "
        val <- intP
        return $ Write pos val


main :: IO ()
main = applyInput programP solveP1 solveP2