module Lib
    ( someFunc
    ) where

import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Lexer hiding (space)
import System.Environment

type Numeral = Int
type Variable = String
data Aexp = LitAexp Numeral
          | VarAexp Variable
          | SumAexp Aexp Aexp
          | ProdAexp Aexp Aexp
          | DiffAexp Aexp Aexp
data Bexp = LitBexp Bool
          | EqBexp Aexp Aexp
          | LeqBexp Aexp Aexp
          | NegBexp Bexp
          | ConjBexp Bexp Bexp
data Stm = AssignStm Variable Aexp
         | SeqStm Stm Stm
         | CondStm Bexp Stm Stm
         | WhileStm Bexp Stm

instance Show Stm where
  show stm = prettyPrintStm stm

someFunc :: IO ()
someFunc = do
  input <- head <$> getArgs
  parseTest parseStm input

parseNumeral :: Parser Numeral
parseNumeral = fmap fromInteger decimal

parseVariable :: Parser Variable
parseVariable = do
  hd <- letterChar
  tl <- many alphaNumChar
  return (hd:tl)

parseAexp :: Parser Aexp
parseAexp = parseLitAexp
        <|> parseVarAexp
        <|> parseSumAexp
        <|> parseProdAexp
        <|> parseDiffAexp
        <?> "arithmatic expression"

parseLitAexp :: Parser Aexp
parseLitAexp = LitAexp <$> parseNumeral

parseVarAexp :: Parser Aexp
parseVarAexp = VarAexp <$> parseVariable

parseSumAexp :: Parser Aexp
parseSumAexp = do
  exp1 <- parseAexp
  space
  string "+"
  space
  exp2 <- parseAexp
  return (SumAexp exp1 exp2)

parseProdAexp :: Parser Aexp
parseProdAexp = do
  exp1 <- parseAexp
  space
  string "*"
  space
  exp2 <- parseAexp
  return (ProdAexp exp1 exp2)

parseDiffAexp :: Parser Aexp
parseDiffAexp = do
  exp1 <- parseAexp
  space
  string "-"
  space
  exp2 <- parseAexp
  return (DiffAexp exp1 exp2)

parseBexp :: Parser Bexp
parseBexp = parseLitBexp
        <|> parseEqBexp
        <|> parseLeqBexp
        <|> parseNegBexp
        <|> parseConjBexp
        <?> "boolean expression"

parseLitBexp :: Parser Bexp
parseLitBexp = parseTrue <|> parseFalse

parseTrue :: Parser Bexp
parseTrue = do
  string "True"
  return (LitBexp True)

parseFalse :: Parser Bexp
parseFalse = do
  string "False"
  return (LitBexp False)

parseEqBexp :: Parser Bexp
parseEqBexp = do
  exp1 <- parseAexp
  space
  string "="
  space
  exp2 <- parseAexp
  return (EqBexp exp1 exp2)

parseLeqBexp :: Parser Bexp
parseLeqBexp = do
  exp1 <- parseAexp
  space
  string "<="
  space
  exp2 <- parseAexp
  return (LeqBexp exp1 exp2)

parseNegBexp :: Parser Bexp
parseNegBexp = do
  char '~'
  space
  exp <- parseBexp
  return (NegBexp exp)

parseConjBexp :: Parser Bexp
parseConjBexp = do
  exp1 <- parseBexp
  space
  exp2 <- parseBexp
  return (ConjBexp exp1 exp2)

parseStm :: Parser Stm
parseStm = parseAssignStm
       <|> parseSeqStm
       <|> parseCondStm
       <|> parseWhileStm
       <?> "statement"

parseAssignStm :: Parser Stm
parseAssignStm = do
  var <- parseVariable
  space
  string ":="
  space
  exp <- parseAexp
  return (AssignStm var exp)

parseSeqStm :: Parser Stm
parseSeqStm = do
  stm1 <- parseStm
  space
  string ";"
  space
  stm2 <- parseStm
  return (SeqStm stm1 stm2)

parseCondStm :: Parser Stm
parseCondStm = do
  string "if"
  space
  exp <- parseBexp
  space
  string "then"
  space
  stm1 <- parseStm
  space
  string "else"
  space
  stm2 <- parseStm
  return (CondStm exp stm1 stm2)

parseWhileStm :: Parser Stm
parseWhileStm = do
  string "while"
  space
  exp <- parseBexp
  space
  string "do"
  space
  stm <- parseStm
  return (WhileStm exp stm)

prettyPrintAexp :: Aexp -> String
prettyPrintAexp (LitAexp n) = show n
prettyPrintAexp (VarAexp var) = var
prettyPrintAexp (SumAexp exp1 exp2) = "(" ++ prettyPrintAexp exp1 ++ " + " ++ prettyPrintAexp exp2 ++ ")"
prettyPrintAexp (ProdAexp exp1 exp2) = prettyPrintAexp exp1 ++ " * " ++ prettyPrintAexp exp2
prettyPrintAexp (DiffAexp exp1 exp2) = "(" ++ prettyPrintAexp exp1 ++ " - " ++ prettyPrintAexp exp2 ++ ")" 

prettyPrintBexp :: Bexp -> String
prettyPrintBexp (LitBexp val) = show val
prettyPrintBexp (EqBexp exp1 exp2) = prettyPrintAexp exp1 ++ " = " ++ prettyPrintAexp exp2
prettyPrintBexp (LeqBexp exp1 exp2) = prettyPrintAexp exp1 ++ " <= " ++ prettyPrintAexp exp2
prettyPrintBexp (NegBexp exp) = "~(" ++ prettyPrintBexp exp ++ ")"
prettyPrintBexp (ConjBexp exp1 exp2) = prettyPrintBexp exp1 ++ " & " ++ prettyPrintBexp exp2

prettyPrintStm :: Stm -> String
prettyPrintStm (AssignStm var exp) = var ++ " := " ++ prettyPrintAexp exp
prettyPrintStm (SeqStm stm1 stm2) = prettyPrintStm stm1 ++ "; " ++ prettyPrintStm stm2
prettyPrintStm (CondStm exp stm1 stm2) = "if " ++ prettyPrintBexp exp ++ " then " ++ prettyPrintStm stm1 ++ " else " ++ prettyPrintStm stm2
prettyPrintStm (WhileStm exp stm) = "while " ++ prettyPrintBexp exp ++ " do " ++ prettyPrintStm stm
