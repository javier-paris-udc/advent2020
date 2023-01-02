module Main where


import           AoC                      (applyInput, intP)
import           Control.Monad.Loops      (whileM_)
import           Control.Monad.State.Lazy (State, execState, gets, modify)
import           Data.HashMap.Strict      (HashMap, (!))
import qualified Data.HashMap.Strict      as Map
import           Data.HashSet             (HashSet)
import qualified Data.HashSet             as Set
import           Data.Function            ((&))
import           Data.List                (find)
import           Data.Maybe               (mapMaybe)
import           Text.Parsec              (choice, newline, sepEndBy, string)
import           Text.Parsec.String       (Parser)


data Inst =
      Nop Int
    | Acc Int
    | Jmp Int
 deriving (Show, Eq)

type Program   = HashMap Int Inst
data EvalState = EvalState {ip    :: Int
                           ,acc   :: Int
                           ,prog  :: Program
                           ,len   :: Int
                           ,execd :: HashSet Int}
                 deriving (Show, Eq)


loopOrEnd :: State EvalState Bool
loopOrEnd = do
    n       <- gets ip
    old     <- gets (Set.member n . execd)
    progLen <- gets len
    modify (\st -> st {execd = Set.insert n (execd st)})
    return $ not old && (n < progLen)

run :: State EvalState ()
run = do
    n    <- gets ip
    inst <- gets ((! n) . prog)
    case inst of
        Nop _ -> modify (\st -> st { ip = ip st + 1})
        Acc v -> modify (\st -> st { ip = ip st + 1, acc = acc st + v})
        Jmp j -> modify (\st -> st { ip = ip st + j})

runProg :: Program -> EvalState
runProg program = execState (whileM_ loopOrEnd run) state0
  where
    state0 = EvalState {ip    = 0
                       ,acc   = 0
                       ,prog  = program
                       ,len   = Map.size program
                       ,execd = Set.empty}


solveP2 :: Program -> Int
solveP2 program =
      programs
    & map runProg
    & find (\st -> ip st >= len st)
    & maybe 0 acc
  where
    programs      = mapMaybe instToState [0..Map.size program - 1]
    instToState i =
        case program ! i of
            Nop j -> Just $ Map.insert i (Jmp j) program
            Acc _ -> Nothing
            Jmp j -> Just $ Map.insert i (Nop j) program


solveP1 :: Program -> Int
solveP1 = acc . runProg


programP :: Parser Program
programP = do
    insts <- instP `sepEndBy` newline
    return $ Map.fromList (zip [0..] insts)
  where
    instP = choice [string "nop " >> Nop <$> intP
                   ,string "acc " >> Acc <$> intP
                   ,string "jmp " >> Jmp <$> intP]


main :: IO ()
main = applyInput programP solveP1 solveP2