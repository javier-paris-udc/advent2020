module Main where


import           AoC                 (applyInput, commaSepP, intP)
import           Data.Bifunctor      (second)
import           Data.Function       ((&))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.List           (isPrefixOf, transpose)
import           Text.Parsec         (many1
                                     ,newline
                                     ,noneOf
                                     ,sepBy1
                                     ,sepEndBy
                                     ,sepEndBy1
                                     ,spaces
                                     ,string)
import           Text.Parsec.String  (Parser)


type Ticket   = [Int]
type Interval = (Int, Int)
type Rule     = (String, [Interval])


inRange :: Int -> Interval -> Bool
inRange n (st, end) = n >= st && n <= end


findValidRules :: [Rule] -> [Int] -> [String]
findValidRules rules vals =
    map fst $ filter (\(_, ivals) -> all (\v -> any (inRange v) ivals) vals) rules


iterateFind :: [Rule] -> [(Int, [Int])] -> HashMap Int String -> HashMap Int String
iterateFind rules vals ruleMap
    | null rules = ruleMap
    | otherwise  = let compatible = map (second $ findValidRules rules) vals
                       singles    = filter ((==1) . length . snd) compatible
                       ruleNames  = map (head . snd) singles
                       fields     = map fst singles
                    in iterateFind (filter (not. (`elem` ruleNames) . fst) rules)
                                   (filterPositions fields)
                                   (insert singles ruleMap)
  where
    filterPositions fields = filter (not. (`elem` fields) . fst) vals
    insert singles ruleM =
        case singles of
            []             -> ruleM
            (i, [name]):ss -> insert ss (Map.insert i name ruleM)
            _              -> undefined



solveP2 :: ([Rule], Ticket, [Ticket]) -> Int
solveP2 (rules, myTicket, tickets) =
      fieldNames
    & Map.filter ("departure" `isPrefixOf`)
    & Map.keys
    & map (myTicket !!)
    & product
  where
    validTickets = myTicket : filter (all (\n -> any (n `inRange`) allIntervals)) tickets
    allIntervals = concatMap snd rules
    fieldNames   = iterateFind rules (zip [0..] $ transpose validTickets) Map.empty


solveP1 :: ([Rule], Ticket, [Ticket]) -> Int
solveP1 (rules, _, tickets) =
    sum $ filter (\n -> not (any (inRange n) allIntervals)) allNums
  where
    allIntervals = concatMap snd rules
    allNums      = concat tickets


ticketsP :: Parser ([Rule], Ticket, [Ticket])
ticketsP = do
    rules    <- ruleP `sepEndBy1` newline
    _        <- newline
    _        <- string "your ticket:"
    _        <- newline
    myTicket <- ticketP
    _        <- spaces
    _        <- string "nearby tickets:"
    _        <- newline
    tickets  <- ticketP `sepEndBy` newline
    return (rules, myTicket, tickets)
  where
    ticketP   = intP `sepBy1` commaSepP
    ruleP     = do
        name      <- many1 (noneOf [':','\n'])
        _         <- string ": "
        intervals <- intervalP `sepBy1` string " or "
        return (name, intervals)
    intervalP = do
        start <- intP
        _     <- string "-"
        end   <- intP
        return (start, end)


main :: IO ()
main = applyInput ticketsP solveP1 solveP2