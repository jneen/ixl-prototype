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
import Control.Applicative ((<$>), (*>), (<*))
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

data Definition = Let (String, Term)
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
parseIxl = parse expr
parseLibrary = parse library

-- main parsers

library :: Parser Library
library = Library <$> many definition <* eof

term :: Parser Term
term = lambda <|> paren <|> atom

-- whitespaces and comments
inlineWhitespace = (many $ (char '\\' >> char '\n') <|> space) >> return ()
whitespace = inlineWhitespace >> many eol
lexeme p = p << inlineWhitespace
comment = char '#' >> many (noneOf "\n") >> optional (char '\n')
eol = (comment <|> (oneOf "\n;" >> inlineWhitespace)) >> return ()

paren = (lexeme (char '(') >> many eol) >> expr << (lexeme (char ')'))

barewordTerminators = " \n\t|=>#;])}"

bare          = many1 $ noneOf barewordTerminators
auto          = bare

identifier    = many (alphaNum <|> oneOf "_-")
optIdentifier = try identifier <|> return ""
symbolic      = char ':' >> identifier

stringLiteral = fmap StringLiteral $ char '\'' >> auto
variable      = fmap Variable      $ char '$'  >> identifier
number        = fmap Number        $ fmap read $ many1 digit
word          = fmap Word          $ bare

atom :: Parser Term
atom = lexeme $
       variable
   <|> number
   <|> stringLiteral
   <|> word

letExpr = do
  def <- definition
  ex <- expr
  return $ Define def ex

-- TODO: add chains, implicit lambdas, bare exprs, etc
expr :: Parser Term
expr = letExpr <|> do
  segment <- foldl1' Apply <$> many1 term
  chain <- optionMaybe (lexeme (char '>') *> many eol)
  case chain of
       Nothing -> return segment
       Just _ -> (`Apply` segment) <$> expr

lambda :: Parser Term
lambda = lbrack >> body << rbrack <?> "lambda"
  where
    lbrack = lexeme (char '[') >> many eol
    rbrack = lexeme (char ']')

    arrow = lexeme (string "=>") >> many eol

    body = fmap Lambda $ many binding

    binding = do
      pat <- pattern
      arrow
      ex <- expr
      many eol
      return (pat, ex)

pattern :: Parser Pattern
pattern = varPattern <|> enumPattern

varPattern = fmap VariablePattern $ lexeme $ char '%' >> identifier
enumPattern = do
  name <- char '.' >> identifier
  pats <- many pattern
  return $ EnumPattern Nothing name pats

definition :: Parser Definition
definition = do
  lexeme (char '+')
  name <- lexeme identifier
  lexeme (char '=')
  val <- expr
  many eol
  return $ Let (name, val)

infixl 1 <<
(<<) :: (Monad m) => m a -> m b -> m a
x << y = do { res <- x; y; return res }
