module Main where


import           AoC                 (applyInput, sep, blanksP, intP)
import           Data.HashMap.Strict (HashMap, (!))
import           Text.Parsec.String  (Parser)
import qualified Data.HashMap.Strict as Map
import           Text.Parsec         ((<|>)
                                     ,between
                                     ,char
                                     ,letter
                                     ,many1
                                     ,newline
                                     ,noneOf
                                     ,sepBy1
                                     ,sepEndBy1)
import           Data.List           (isPrefixOf)
import           Data.Function       ((&))


data RuleToken = Str String | Rule Int deriving (Show, Eq)
type RuleOpt   = [RuleToken]
type Rule      = [RuleOpt]


partialMatch :: HashMap Int Rule -> Int -> String -> [String]
partialMatch rules n str =
    concatMap (tryOpt str) (rules ! n)
  where
    tryOpt st opt =
        case opt of
            []         -> [st]
            tkn : tkns -> tryToken tkn st >>= (`tryOpt` tkns)

    tryToken tkn st =
        case tkn of
            Rule i                  -> partialMatch rules i st
            Str  s
                | s `isPrefixOf` st -> [drop (length s) st]
                | otherwise         -> []


matches :: HashMap Int Rule -> String -> Bool
matches rules = any null . partialMatch rules 0


solveP2 :: (HashMap Int Rule, [String]) -> Int
solveP2 (rules, strs) = length $ filter (matches newRules) strs
  where
    newRules = rules
             & Map.insert 8  [[Rule 42], [Rule 42, Rule 8]]
             & Map.insert 11 [[Rule 42, Rule 31], [Rule 42, Rule 11, Rule 31]]


solveP1 :: (HashMap Int Rule, [String]) -> Int
solveP1 (rules, strs) = length $ filter (matches rules) strs


rulesMsgsP :: Parser (HashMap Int Rule, [String])
rulesMsgsP = do
    rules    <- rulesP
    _        <- newline
    messages <- many1 letter `sepEndBy1` newline
    return (Map.fromList rules, messages)
  where
    rulesP = ruleP `sepEndBy1` newline
    ruleP  = do
        ruleN <- intP
        _     <- char ':'
        _     <- blanksP
        rules <- ruleOptP `sepBy1` sep "|"
        return (ruleN, rules)
    ruleOptP   = ruleTokenP `sepEndBy1` blanksP
    ruleTokenP = Str  <$> between (char '"') (char '"') (many1 $ noneOf "\"")
             <|> Rule <$> intP


main :: IO ()
main = applyInput rulesMsgsP solveP1 solveP2