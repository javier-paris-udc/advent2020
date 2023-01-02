module Main where


import AoC                (applyInput)
import Data.Char          (isDigit, isHexDigit)
import Text.Parsec        ((<|>)
                          ,alphaNum
                          ,char
                          ,choice
                          ,many1
                          ,newline
                          ,sepEndBy1
                          ,space
                          ,string
                          ,try)
import Text.Parsec.String (Parser)


data Field = Byr
          | Iyr | Eyr | Hgt | Hcl | Ecl | Pid | Cid
    deriving (Show, Eq)

type Passport = [(Field, String)]


validField :: (Field, String) -> Bool
validField (field, str) =
    case field of
        Byr -> number 1920 2002
        Iyr -> number 2010 2020
        Eyr -> number 2020 2030
        Hgt -> checkHeight
        Hcl -> checkHair
        Ecl -> str `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        Pid -> all isDigit str && length str == 9
        Cid -> True
  where
    number :: Int -> Int -> Bool
    number low high = let n = read str in all isDigit str && n >= low && n <= high
    checkHeight     = let (nStr, unit) = span isDigit str
                          n :: Int     = read nStr
                      in case unit of
                        "in" -> n >= 59  && n <= 76
                        "cm" -> n >= 150 && n <= 193
                        _    -> False
    checkHair       = let (hash, rest) = splitAt 1 str
                      in hash == "#" && length rest == 6 && all isHexDigit rest


isValid :: Passport -> Bool
isValid p = allFields (map fst p)
         && all validField p


allFields :: [Field] -> Bool
allFields fields = all (`elem` fields) [Byr, Iyr, Eyr, Hgt, Hcl, Ecl, Pid]


solveP2 :: [Passport] -> Int
solveP2 =
    length . filter isValid


solveP1 :: [Passport] -> Int
solveP1 =
    length . filter allFields . map (map fst)


fieldNameP :: Parser Field
fieldNameP =
    choice
        (try <$> [string "byr" >> return Byr
                 ,string "iyr" >> return Iyr
                 ,string "eyr" >> return Eyr
                 ,string "hgt" >> return Hgt
                 ,string "hcl" >> return Hcl
                 ,string "ecl" >> return Ecl
                 ,string "pid" >> return Pid
                 ,string "cid" >> return Cid
                 ])


fieldP :: Parser (Field, String)
fieldP =
    do
        field <- fieldNameP
        _     <- char ':'
        str   <- many1 (alphaNum <|> char '#')
        return (field, str)


passportP :: Parser Passport
passportP =
    fieldP `sepEndBy1` space


passportsP :: Parser [Passport]
passportsP = passportP `sepEndBy1` newline


main :: IO ()
main = applyInput passportsP solveP1 solveP2
