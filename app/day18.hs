module Main where


import AoC                (applyInput1, intP)
import Text.Parsec        ((<|>)
                          ,between
                          ,char
                          ,choice
                          ,optionMaybe
                          ,newline
                          ,sepEndBy
                          ,string
                          ,try)
import Text.Parsec.String (Parser)


data Oper = Add | Prod deriving (Show, Eq)
data Exp = V Int | Op Oper Exp Exp deriving (Show, Eq)


eval :: Exp -> Int
eval e = case e of
    V x         -> x
    Op op e1 e2 -> opFun op (eval e1) (eval e2)
  where
    opFun op =
        case op of
            Add  -> (+)
            Prod -> (*)


solve :: [Exp] -> Int
solve = sum . map eval


numP :: Parser Exp
numP = V <$> intP


prodP :: Parser Oper
prodP = try (string " * " >> return Prod)


addP :: Parser Oper
addP = try (string " + " >> return Add)


expsPrioP :: Parser [Exp]
expsPrioP = expPrioP `sepEndBy` newline
  where
    expPrioP = do
        e1   <- baseAddP
        rest <- expPrioR
        case rest of
            Nothing -> return e1
            Just f  -> return $ f e1
    expPrioR = optionMaybe $ do
        op <- prodP
        e  <- baseAddP
        rest <- expPrioR
        case rest of
            Nothing -> return (\e1 -> Op op e1 e)
            Just f  -> return (\e1 -> f $ Op op e1 e)
    baseAddP = do
        choice [try addExpP, try numP, try parensP]
    addExpP = do
        e1 <- baseP
        op <- addP
        e2 <- baseP
        rest <- addExpPrioR
        case rest of
            Nothing -> return $ Op op e1 e2
            Just f  -> return $ f $ Op op e1 e2
    addExpPrioR = optionMaybe $ do
        op   <- addP
        e    <- baseP
        rest <- addExpPrioR
        case rest of
            Nothing -> return (\e1 -> Op op e1 e)
            Just f  -> return (\e1 -> f $ Op op e1 e)

    baseP = numP <|> parensP
    parensP = between (char '(') (char ')') expPrioP


expsLeftAssocP :: Parser [Exp]
expsLeftAssocP = expP `sepEndBy` newline
  where
    expP = do
        e1   <- baseP
        op   <- opP
        e2   <- baseP
        rest <- expR
        case rest of
            Nothing -> return $ Op op e1 e2
            Just f  -> return $ f (Op op e1 e2)

    expR = optionMaybe $ do
        op <- opP
        e  <- baseP
        rest <- expR
        case rest of
            Nothing -> return (\e1 -> Op op e1 e)
            Just f  -> return (\e1 -> f $ Op op e1 e)

    baseP = (V <$> intP) <|> parensP
    opP = try (string " + " >> return Add) <|> try (string " * " >> return Prod)
    parensP = between (char '(') (char ')') expP


main :: IO ()
main = do
    applyInput1 expsLeftAssocP solve
    applyInput1 expsPrioP solve