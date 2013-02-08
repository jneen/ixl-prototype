{-# LANGUAGE NoMonomorphismRestriction #-}

module Ixl.Syntax (
  Term(..),
  Command(..),
  Program(..),
  parseIxl,
) where

import qualified Data.DList as D
import Data.Map ((!))
import Data.List (intercalate)
import Text.ParserCombinators.Parsec
import Control.Monad.Writer
import Control.Applicative ((<$>))
import Data.Monoid (mempty, (<>), mconcat)
import Data.Char (chr)
import Numeric (readHex)
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
                 c'target :: Maybe Term,
                 c'terms :: [Term],
                 c'pipe :: Maybe Command
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

barewordTerminators = " \n\t|#;])"

bare          = many1 $ noneOf barewordTerminators
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
interp        = fmap Interp        $ char '"' >> (interpBraces <|> interpBare)
variable      = fmap Variable      $ char '$'  >> identifier
flag          = fmap Flag          $ char '-'  >> identifier
bareword      = fmap Bareword      $ auto
typedBareword = fmap TypedBareword $ typed
symbol        = fmap Symbol        $ symbolic


atom :: Parser Term
atom = (variable
   <|> flag
   <|> stringLiteral
   <|> interp
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

type DString = D.DList Char
type BracesState a = (a, Integer)

makeBraces :: (Monoid a) => [(Integer, Parser a)] -> Parser a
makeBraces config = do
  char '{'
  countBraces (mempty, 1)
  where
    -- counter :: (Monoid a) => (Integer, Parser a) -> BracesState a -> Parser a
    counter (delta, parser) (buf, count) = do
      new <- parser
      countBraces (buf <> new, count + delta)

    with = flip ($)

    -- alternatives :: (Monoid a) => BracesState a -> Parser a
    alternatives st = foldl1 (<|>) $ map (with st . counter) config

    -- countBraces :: (Monoid a) => BracesState a -> Parser a
    countBraces (s, 1) = (const s <$> char '}') <|> alternatives (s, 1)
    countBraces x = alternatives x

times :: Parser a -> Integer -> Parser [a]
times _ 0 = return []
times p n = do
  next <- p
  (next:) <$> times p (n-1)

braces :: Parser String
braces = fmap D.toList $ makeBraces $ [
  (1,  return <$> char '{'),
  (-1, return <$> char '}'),
  (0,  D.fromList <$> many1 (noneOf "{}")) ]

interpEscape :: Parser Term
interpEscape = StringLiteral . return <$> do
  char '\\'
  common <|> ch8bit <|> ch16bit <|> ch32bit <|> anyChar

  where
    common = (char 'n'  >> return '\n')
         <|> (char 't'  >> return '\t')
         <|> (char 'r'  >> return '\r')

    hexDigitsChar i = chr . fst . head . readHex <$> times hexDigit i

    ch8bit  = char 'x' >> hexDigitsChar 2
    ch16bit = char 'u' >> hexDigitsChar 4
    ch32bit = char 'U' >> hexDigitsChar 8

interpBraces :: Parser [Term]
interpBraces = makeBraces [
  (1,  return . StringLiteral . return <$> char '{'),
  (-1, return . StringLiteral . return <$> char '}'),
  (0,  return <$> interpEscape),
  (0,  return <$> interpDollar),
  (0,  return . StringLiteral <$> many1 (noneOf "{}\\$")) ]

interpBare :: Parser [Term]
interpBare = many (interpDollar <|> interpEscape <|> stringComponent)
  where
    stringComponent = fmap StringLiteral (many1 (noneOf ('$':'\\':barewordTerminators)))

interpDollar :: Parser Term
interpDollar = char '$' >> (subst <|> fmap Variable identifier)

consolidateInterp :: [Term] -> [Term]
consolidateInterp ((StringLiteral s1):(StringLiteral s2):tail)
  = consolidateInterp ((StringLiteral (s1 ++ s2)):tail)
consolidateInterp (x:xs) = x:(consolidateInterp xs)
consolidateInterp [] = []

infixl 1 <<
(<<) :: (Monad m) => m a -> m b -> m a
x << y = do { res <- x; y; return res }
