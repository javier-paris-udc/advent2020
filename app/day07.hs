module Main where


import           AoC                 (applyInput, blanksP, intP)
import           Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as Map
import           Text.Parsec         ((<|>)
                                     ,letter
                                     ,many1
                                     ,newline
                                     ,sepBy1
                                     ,sepEndBy
                                     ,string
                                     ,try)
import           Text.Parsec.String  (Parser)


type Color   = String
type RuleMap = HashMap Color [(Color, Int)]


bagsInside :: RuleMap -> String -> Int
bagsInside rules bag = sum (map (\(color, n) -> n * (1 + bagsInside rules color)) content)
  where
    content = rules ! bag


reachable :: RuleMap -> Color -> (Color, Int) -> Bool
reachable rules goal (color, _) =
    goal == color || any (reachable rules goal) (rules ! color)


solveP2 :: RuleMap -> Int
solveP2 rules = bagsInside rules "shiny gold"


solveP1 :: RuleMap -> Int
solveP1 rules = Map.size $ Map.filter (any (reachable rules "shiny gold")) rules


rulesP :: Parser RuleMap
rulesP = Map.fromList <$> ruleP `sepEndBy` newline
  where
    ruleP = do
        outerBag  <- colorP
        _         <- string " bags contain "
        innerBags <- innerBagsP
        return (outerBag, innerBags)

    colorP = do
        word1 <- many1 letter
        _     <- string " "
        word2 <- many1 letter
        return (word1 ++ " " ++ word2)

    innerBagsP = do
        (string "no other bags." >> return []) <|> innerBags1P

    innerBags1P = do
        bags <- innerBagP `sepBy1` string ", "
        _    <- string "."
        return bags

    innerBagP = do
        n   <- intP
        _   <- blanksP
        bag <- colorP
        _   <- try (string " bags") <|> try (string " bag")
        return (bag, n)


main :: IO ()
main = applyInput rulesP solveP1 solveP2