{-# LANGUAGE NoMonomorphismRestriction, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE LambdaCase #-}

module Ixl.Syntax (
  parseIxl,
  parseLibrary,
  Term(..),
  Definition(..),
  Pattern(..),
  Type(..),
  Library(..),
) where

import Data.Map ((!))
import Data.List (intercalate, foldl')
import Text.ParserCombinators.Parsec
import Data.Traversable (Traversable)
import Data.Foldable (Foldable)
import Control.Monad.Writer
import Control.Applicative ((<$>), (*>), (<*), (<*>), pure)
import Data.Monoid (mempty, (<>), mconcat)
import Data.Char (chr)
import Numeric (readHex)
import qualified Data.Map as Map

{---- AST ----}

-- expressions
data Term a = StringLiteral String
            | Number Int
            | Variable a
            | Word String
            | CommandWord String
            | Lambda [(Pattern a, Term a)]
            | Tuple (Term a) (Term a)
            | Apply (Term a) (Term a)
            | Pipe (Term a) (Term a)
            | Define [Definition a] (Term a)
            | Annotated a (Term a)
            deriving(Show, Eq, Functor, Foldable, Traversable)

data Definition a = Let a (Term a)
                  | Struct [(a, Type a)]
                  | Enum [(a, Type a)]
                  deriving(Show, Eq, Functor, Foldable, Traversable)

-- types

data Pattern a = EnumPattern (Maybe a) a [Pattern a]
               | StructPattern a [(a, Pattern a)]
               | VariablePattern a
               deriving(Show, Eq, Functor, Foldable, Traversable)

data Type a = TyCon a [Type a]
              deriving(Show, Eq, Functor, Foldable, Traversable)

data Library a = Library [Definition a] deriving(Show, Eq, Functor, Foldable, Traversable)

{---- PARSERS ----}

-- exported function
parseIxl = parse (expr <* eof)
parseLibrary = parse library

-- main parsers

library :: Parser (Library String)
library = Library <$> many definition <* eof

term :: Parser (Term String)
term = do
  base <- singleTerm
  optionMaybe (char ',' *> ws) >>= \case
    Nothing -> return base
    Just _ -> Tuple <$> return base <*> term

singleTerm :: Parser (Term String)
singleTerm = lambda <|> paren <|> atom

-- whitespaces and comments
ws = many ((char '\\' *> char '\n') <|> space) *> pure ()
ws' = ws <* eols
comment = char '#' *> many (noneOf "\n") *> optional (char '\n')
eol = (comment <|> (oneOf "\n;" *> ws))
eols = many eol *> pure ()

paren = char '(' *> ws' *> expr <* char ')' <* ws

barewordTerminators = " \n\t|=>,#;])}"

bare          = many1 $ noneOf barewordTerminators
auto          = bare

identifier    = many (alphaNum <|> oneOf "_-")
optIdentifier = try identifier <|> return ""
symbolic      = char ':' *> identifier

stringLiteral = StringLiteral <$> (char '\'' *> auto)
variable      = Variable      <$> (char '$'  *> identifier)
number        = Number        <$> read <$> many1 digit
word          = Word          <$> bare

atom :: Parser (Term String)
atom = (
    variable
    <|> number
    <|> stringLiteral
    <|> word
  ) <* ws <?> "an atom"

letExpr = Define <$> many1 definition <*> expr

-- TODO: add chains, implicit lambdas, bare exprs, etc
expr :: Parser (Term String)
expr = letExpr <|> do
  start <- term
  let command = case start of
                     Word w -> CommandWord w
                     _ -> start
  segment <- foldl' Apply command <$> many term
  appChain segment <|> pipeChain segment <|> pure segment

appChain segment = Apply <$> (char '>' *> ws' *> expr) <*> pure segment
pipeChain segment = Pipe <$> (char '|' *> ws' *> expr) <*> pure segment

lambda :: Parser (Term String)
lambda = lbrack *> body <* rbrack <?> "lambda"
  where
    lbrack = char '[' *> ws'
    rbrack = char ']' *> ws

    arrow = string "=>" *> ws'
    body = Lambda <$> many binding

    binding = (,) <$> (pattern <* arrow) <*> (expr <* eols)

pattern :: Parser (Pattern String)
pattern = varPattern <|> enumPattern

varPattern = VariablePattern <$> (char '%' *> identifier <* ws)
enumPattern = EnumPattern Nothing <$> (char '.' *> identifier) <*> many pattern

definition :: Parser (Definition String)
definition = Let <$> (char '+' *> ws *> identifier <* ws)
                 <*> (char '=' *> ws' *> expr <* eols)
