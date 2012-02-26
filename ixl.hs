{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad
import Text.ParserCombinators.Parsec

-- TODO: there's probably a better way to do this
braces = do
  char '{'
  result <- bracesInternal
  char '}'
  return result

  where
    bracesInternal = do
      intro <- atom
      others <- many piece
      return $ intro ++ (foldl (++) "" others)

      where
        atom = many $ noneOf "{}"
        inner = do
          inside <- braces
          return $ "{" ++ inside ++ "}"

        piece = do
          i <- inner
          a <- atom
          return $ i ++ a

bareword = liftM2 (:) letter (many $ noneOf " \n\t|#;")
identifier = braces <|> bareword

stringLiteral = char '\'' >> identifier
variable      = char '.'  >> identifier
keyword       = char '-'  >> identifier

atom = variable
   <|> stringLiteral
   <|> keyword
   <|> bareword
   <?> "atomic expression"

expr = atom -- <|> lambda etc.
          <?> "expression"

spacech = string "\\\n" <|> string " " <?> "space"
whitespace = many1 spacech <?> "whitespace"
optWhitespace = many spacech

comment = do
  char '#'
  many $ noneOf "\n"

term = do
  optWhitespace
  char ';' <|> eol
  optWhitespace

  where
    eol = do
      optional comment
      char '\n'

terms = optWhitespace >> many term >> optWhitespace

command = do
  liftM2 (:) expr rest
  where
    rest = do
      try (whitespace >> command) <|> return []

pipe = do
  terms
  optWhitespace
  char '|'
  optWhitespace

pipeChain = do
  liftM2 (:) command rest
  where rest = do
          try (pipe >> pipeChain) <|> return []

code = sepBy pipeChain terms

program = endBy eof code

-- utility function for ghci
p parser = parse parser "(passed-in)"

main = do
  c <- getContents
  case parse program "(stdin)" c of
       Left e -> do putStrLn "Error parsing input:"
                    print e
       Right r -> mapM_ print r
