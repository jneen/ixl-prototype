{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad
import Text.ParserCombinators.Parsec

-- utility function for ghci
p parser = parse parser "(passed-in)"

-- ast

-- expressions
data ASTExpr = StringNode String
             | VariableNode String
             | SymbolNode String
             | LambdaNode ASTProgram
          -- | TableNode [(ASTExpr, ASTExpr)]
          -- | ListNode [ASTExpr]
             deriving(Show)

-- compound types
data ASTCommand = CommandNode [ASTExpr]
                  deriving(Show)

data ASTPipeChain = PipeChainNode [ASTCommand]
                    deriving(Show)

data ASTProgram = ProgramNode [ASTPipeChain]
                  deriving(Show)

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

bareword = liftM2 (:) letter (many $ noneOf " \n\t|#;]")
identifier = braces <|> bareword
optIdentifier = try identifier <|> return ""

stringLiteral = fmap StringNode $ char '\'' >> optIdentifier
variable      = fmap VariableNode $ char '.'  >> optIdentifier
keyword       = fmap SymbolNode $ char '-'  >> identifier

atom = variable
   <|> stringLiteral
   <|> keyword
   <|> (fmap StringNode bareword)
   <?> "atomic expression"

lambda = fmap LambdaNode $ between (char '[') (char ']') code

expr = atom <|> lambda

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

btSepBy p sep = result
  where
    result = do -- liftM2 (:) p rest
      h <- p
      t <- rest
      return (h:t)
    rest = (try (sep >> result)) <|> return []

command = fmap CommandNode $ btSepBy expr whitespace

pipe = do
  terms
  optWhitespace
  char '|'
  optWhitespace

pipeChain = fmap PipeChainNode $ btSepBy command pipe

code = fmap ProgramNode $ do
  terms
  r <- sepBy pipeChain terms
  terms
  return r

program = endBy eof code

main = do
  c <- getContents
  case parse program "(stdin)" c of
       Left e -> do putStrLn "Error parsing input:"
                    print e
       Right r -> mapM_ print r
