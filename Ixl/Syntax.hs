{-# LANGUAGE NoMonomorphismRestriction #-}

module Ixl.Syntax (
  Term(..),
  Command(..),
  Program(..),
  parseIxl,
) where

import Ixl.Env

import qualified Data.DList as D
import Data.Map ((!))
import Data.List (intercalate)
import Text.ParserCombinators.Parsec
import Control.Monad.Writer
import Control.Applicative ((<$>))
import qualified Data.Map as Map

{---- AST ----}

-- expressions
data Term = StringLiteral String
          | Symbol String
          | Bareword String
          | TypedBareword (String, String)
          | Variable String
          | Flag String
          | Interp [Term]
          | Block ([Pattern], Program)
          | Subst Program
          deriving(Show)

data Pattern = FlagPattern String
             | VariablePattern (Maybe String, String)
             | Ellipsis Pattern
             deriving(Show)

data Command = Command {
                 getTarget :: Maybe Term,
                 getTerms :: [Term],
                 getPipe :: Maybe Command
               } deriving(Show)

data Program = Program [Command] deriving(Show)

{---- PARSERS ----}

-- exported function
parseIxl = parse program

-- main parsers

program :: Parser Program
program = code << eof

term :: Parser Term
term = atom <|> block <|> subst

-- whitespaces and comments
inlineWhitespace = (many $ (char '\\' >> char '\n') <|> space) >> return ()
whitespace = inlineWhitespace >> many eol
comment = char '#' >> many (noneOf "\n") >> optional (char '\n')
eol = (comment <|> (oneOf "\n;" >> inlineWhitespace)) >> return ()

startBareword = oneOf ":*@" <|> alphaNum
bare          = liftM2 (:) startBareword (many $ noneOf " \n\t|#;]")
auto          = braces <|> bare
identifier    = many (alphaNum <|> oneOf "_-")
optIdentifier = try identifier <|> return ""
symbolic      = char ':' >> identifier

typed = do
  char '%'
  type_ <- identifier
  char ':'
  content <- auto
  return (type_, content)

stringLiteral = fmap StringLiteral $ char '\'' >> auto
variable      = fmap Variable      $ char '$'  >> identifier
flag          = fmap Flag          $ char '-'  >> identifier
bareword      = fmap Bareword      $ auto
typedBareword = fmap TypedBareword $ typed
symbol        = fmap Symbol        $ symbolic


atom :: Parser Term
atom = (variable
   <|> flag
   <|> stringLiteral
   <|> typedBareword
   <|> symbol
   <|> bareword
   )<< inlineWhitespace

block :: Parser Term
block = fmap Block blockBody <?> "lambda"
  where
    blockBody = do
      char '[' >> whitespace
      args <- option [] argSpec
      body <- code
      char ']' >> inlineWhitespace
      return (args, body)

    argSpec :: Parser [Pattern]
    argSpec = do
      char '|' >> inlineWhitespace
      list <- many pattern
      char '|' >> whitespace
      return list

    flagPat = fmap FlagPattern     $ char '-' >> identifier
    varPat  = symbolic >>= \sym -> return (VariablePattern (Nothing, sym))

    typedPat = fmap VariablePattern $ do
      (type_, name) <- typed
      return (Just type_, name)

    -- TODO: ellipsis patterns
    pattern = (flagPat <|> varPat <|> typedPat) << inlineWhitespace

subst :: Parser Term
subst = fmap Subst parens <?> "substitution"
  where parens = do
          char '(' >> whitespace
          prog <- code
          char ')' >> inlineWhitespace
          return prog

code :: Parser Program
code = fmap Program $ many command << whitespace

target :: Parser Term
target = char '@' >> inlineWhitespace >> term

pipe :: Parser Command
pipe = char '|' >> inlineWhitespace >> command

command :: Parser Command
command = do
  target <- optionMaybe target

  -- must be either a target or at
  -- least one term.
  terms <- case target of
                Nothing -> many1 term
                Just _  -> many term

  -- eols are allowed before pipes
  many eol
  pipe <- optionMaybe pipe
  return $ Command target terms pipe

infixl 1 <<
(<<) :: (Monad m) => m a -> m b -> m a
x << y = do { res <- x; y; return res }

type DString = D.DList Char
type BracesState a = (a, Integer)

dbraces :: Parser DString
dbraces = do
  char '{'
  countBraces ((d ""), 1)

  where
    counter :: (Integer, Parser String) -> BracesState DString -> Parser DString
    counter (delta, parser) (buf, count) = do
      new <- parser
      countBraces ((buf ++ d new), (count + delta))

    nonBraces  = counter (0,  many1 (noneOf "{}\\"))
    openBrace  = counter (1,  return <$> char '{')
    closeBrace = counter (-1, return <$> char '}')
    escape     = counter (0,  return <$> (char '\\' >> anyChar))

    alternatives :: BracesState DString -> Parser DString
    alternatives x = foldl1 (<|>) $ map (flip ($) x) [nonBraces, openBrace, closeBrace, escape]

    countBraces :: BracesState DString -> Parser DString
    countBraces (s, 1) = (const s <$> char '}') <|> alternatives (s, 1)
    countBraces x = alternatives x

    d = D.fromList
    (++) = D.append

braces :: Parser String
braces = fmap D.toList dbraces

