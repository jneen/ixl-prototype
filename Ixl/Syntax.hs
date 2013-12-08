{-# LANGUAGE NoMonomorphismRestriction #-}

module Ixl.Syntax (
  parseIxl,
  Term(..),
  Definition(..),
  Pattern(..),
  Type(..),
  Library(..),
) where

import Data.Map ((!))
import Data.List (intercalate, foldl1')
import Text.ParserCombinators.Parsec
import Control.Monad.Writer
import Control.Applicative ((<$>), (*>), (<*), (<*>), pure)
import Data.Monoid (mempty, (<>), mconcat)
import Data.Char (chr)
import Numeric (readHex)
import qualified Data.Map as Map

{---- AST ----}

-- expressions
data Term = StringLiteral String
          | Number Int
          | Variable String
          | Word String
          | Lambda [(Pattern, Term)]
          | Apply Term Term
          | Pipe Term Term
          | Chain Term Term
          | Define Definition Term
          deriving(Show, Eq)

data Definition = Let String Term
                | Struct [(String, Type)]
                | Enum [(String, Type)]
                deriving(Show, Eq)

-- types

data Pattern = EnumPattern (Maybe String) String [Pattern]
             | StructPattern String [(String, Pattern)]
             | VariablePattern String
             deriving(Show, Eq)

data Type = Type Int -- TODO
          deriving(Show, Eq)

data Library = Library [Definition] deriving(Show, Eq)

{---- PARSERS ----}

-- exported function
parseIxl = parse (expr <* eof)
parseLibrary = parse library

-- main parsers

library :: Parser Library
library = Library <$> many definition <* eof

term :: Parser Term
term = lambda <|> paren <|> atom

-- whitespaces and comments
ws = (many $ (char '\\' *> char '\n') <|> space) *> pure ()
ws' = ws <* many eol
comment = char '#' *> many (noneOf "\n") *> optional (char '\n')
eol = (comment <|> (oneOf "\n;" *> ws))

paren = char '(' *> ws' *> expr <* char ')' <* ws

barewordTerminators = " \n\t|=>#;])}"

bare          = many1 $ noneOf barewordTerminators
auto          = bare

identifier    = many (alphaNum <|> oneOf "_-")
optIdentifier = try identifier <|> return ""
symbolic      = char ':' *> identifier

stringLiteral = StringLiteral <$> (char '\'' *> auto)
variable      = Variable      <$> (char '$'  *> identifier)
number        = Number        <$> read <$> many1 digit
word          = Word          <$> bare

atom :: Parser Term
atom = (
    variable
    <|> number
    <|> stringLiteral
    <|> word
  ) <* ws <?> "an atom"

letExpr = Define <$> definition <*> expr

-- TODO: add chains, implicit lambdas, bare exprs, etc
expr :: Parser Term
expr = letExpr <|> do
  segment <- foldl1' Apply <$> many1 term
  chain <- optionMaybe $ char '>' *> ws'
  case chain of
       Nothing -> return segment
       Just _ -> (`Apply` segment) <$> expr

lambda :: Parser Term
lambda = lbrack *> body <* rbrack <?> "lambda"
  where
    lbrack = char '[' *> ws'
    rbrack = char ']' *> ws

    arrow = string "=>" *> ws'

    body = Lambda <$> many binding

    binding = (,) <$> (pattern <* arrow) <*> (expr <* ws')

pattern :: Parser Pattern
pattern = varPattern <|> enumPattern

varPattern = VariablePattern <$> (char '%' *> identifier <* ws)
enumPattern = EnumPattern Nothing <$> (char '.' *> identifier) <*> many pattern

definition :: Parser Definition
definition = Let <$> (char '+' *> ws *> identifier <* ws)
                 <*> (char '=' *> ws *> expr <* ws')
