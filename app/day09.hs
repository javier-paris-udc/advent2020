module Main where


import           AoC                 (applyInput, intP)
import           Text.Parsec.String  (Parser)
import           Text.Parsec         (sepEndBy, spaces)
import qualified Data.Sequence       as Seq
import           Data.Sequence       (Seq ((:<|), Empty, (:|>)))
import           Control.Monad.Loops (whileM_)
import           Control.Monad.State (State, execState, get, put)
import           Data.List           (find, inits, tails)


data XmasSt = XmasSt {q     :: Seq Int
                     ,nums  :: [Int]
                     } deriving (Show, Eq)

sumTo :: Int -> Seq Int -> Bool
sumTo n s =
    case s of
        Empty     -> False
        n1 :<| ns -> elem (n - n1) ns || sumTo n ns


isSum :: State XmasSt Bool
isSum = do
    st <- get
    case nums st of
        []     -> return False
        next:_ -> return $ sumTo next (q st)


step :: State XmasSt ()
step = do
    st <- get
    case q st of
        Empty    -> undefined
        _ :<| ns ->
            case nums st of
                []         -> undefined
                next:nexts -> put $ XmasSt {q     = ns :|> next
                                           ,nums  = nexts}


solveP2 :: [Int] -> Int
solveP2 nums =
    case find ((==n) . sum) sets of
        Nothing -> 0
        Just l  -> maximum l  + minimum l
  where
    n          = solveP1 nums
    sets       = filter (\l -> not (null l || single l)) (concatMap tails $ inits nums)
    single [_] = True
    single _   = False


solveP1 :: [Int] -> Int
solveP1 l = head (nums $ execState (whileM_ isSum step) state0)
  where
    (preamble, rest) = splitAt 25 l
    seq0             = Seq.fromList preamble
    state0           = XmasSt {q = seq0, nums = rest}


numbersP :: Parser [Int]
numbersP = intP `sepEndBy` spaces


main :: IO ()
main = applyInput numbersP solveP1 solveP2