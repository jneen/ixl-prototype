{-# LANGUAGE NoMonomorphismRestriction #-}

import qualified Data.DList as D
import Text.ParserCombinators.Parsec
import Control.Monad.Writer

-- utility function for ghci
p parser = parse parser "(passed-in)"

{---- AST ----}

-- expressions
data ASTExpr = StringNode String
             | VariableNode String
             | SymbolNode String
             | LambdaNode ASTProgram
             | SubstNode ASTProgram
             | SyntaxNode ASTProgram
             | NumberNode String
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

{---- Compiling ----}
class AST a where
  -- AST nodes can be compiled to an instruction list
  scompile :: a -> Writer (D.DList String) ()
  compile :: a -> [String]
  compile = D.toList . snd . runWriter . scompile

emit = tell . D.fromList

compileString parser str =
  case parse parser "(passed-in)" str of
       Left e -> Left e
       Right ast -> Right $ compile ast

instance AST ASTExpr where
  scompile (NumberNode n)    = emit ["push int " ++ n]
  scompile (StringNode s)    = emit ["push str " ++ s]
  scompile (VariableNode "") = emit ["get_context"]
  scompile (VariableNode v)  = emit ["lookup " ++ v]
  scompile (LambdaNode l)    = emit ["lambda (TODO)"]
  scompile (SubstNode s)     = emit ["subst (TODO)"]
  scompile _ = fail "not implemented"

emitMany = foldl (>>) $ return ()

instance AST ASTCommand where
  scompile (CommandNode c) = do
    emitMany $ map scompile c
    emit ["call " ++ (show $ length c)]

instance AST ASTPipeChain where
  scompile (PipeChainNode pc) = do
    emitMany $ map pipe pc
    emit ["pipe " ++ (show $ length pc)]

    where
      pipe pc = scompile pc >> emit ["set_context"]


instance AST ASTProgram where
  scompile (ProgramNode p) = do
    emitMany $ map scompile p

-- TODO: there's probably a better way to do this
dbraces :: GenParser Char st (D.DList Char)
dbraces = do
  char '{'
  result <- bracesInternal
  char '}'
  return result

  where
    bracesInternal = do
      intro <- atom
      others <- many piece
      return $ D.concat (intro:others)

      where
        atom :: GenParser Char st (D.DList Char)
        atom = fmap D.fromList $ many $ do
          escape <|> noneOf "{}"
          where escape = char '\\' >> oneOf "{}\\"

        inner :: GenParser Char st (D.DList Char)
        inner = do
          inside <- dbraces
          return $ (D.fromList "{") `D.append` inside `D.append` (D.fromList "}")

        piece :: GenParser Char st (D.DList Char)
        piece = do
          i <- inner
          a <- atom
          return $ i `D.append` a

braces = fmap D.toList dbraces

bareword      = liftM2 (:) letter (many $ noneOf " \n\t|#;]")
identifier    = braces <|> bareword
optIdentifier = try identifier <|> return ""

number        = fmap NumberNode   $ many1 digit
stringLiteral = fmap StringNode   $ char '\'' >> optIdentifier
variable      = fmap VariableNode $ char '.'  >> optIdentifier
keyword       = fmap SymbolNode   $ char '-'  >> identifier

atom = variable
   <|> stringLiteral
   <|> number
   <|> keyword
   <|> (fmap StringNode bareword)
   <?> "atomic expression"

lambda = fmap LambdaNode brackets <?> "lambda"
  where brackets = between (char '[') (char ']') code

subst = fmap SubstNode parens <?> "substitiution"
  where parens = between (char '(') (char ')') code

syntax = fmap SyntaxNode bareBraces <?> "syntax"
  where bareBraces = between (char '{') (char '}') code

expr = atom <|> lambda <|> subst <|> syntax

spacech = string "\\\n" <|> string " " <?> "space"
whitespace = many1 spacech <?> "whitespace"
optWhitespace = many spacech

comment = do
  char '#'
  many $ noneOf "\n"

-- end-of-line, with optional comments
term = do
  optWhitespace
  char ';' <|> (optional comment >> char '\n')
  optWhitespace

-- optional whitespace-padded eols
terms = do
  optWhitespace
  try (many term) <|> return [] <?> "terms"
  optWhitespace

-- backtracking version of sepBy
-- not quite as performant, but it will backtrack
-- if it finds the separator at the end of the sequence
btSepBy p sep = result
  where
    result = do -- liftM2 (:) p rest
      h <- p
      t <- rest
      return (h:t)
    rest = try (sep >> result)
       <|> return []

command = fmap CommandNode $ btSepBy expr (whitespace <?> "between whitespace")

pipe = do
  terms <?> "pipe terms"
  optWhitespace
  char '|'
  optWhitespace

pipeChain = fmap PipeChainNode $ btSepBy command pipe

code = fmap ProgramNode $ do
  terms <?> "beginning terms"
  r <- try (btSepBy pipeChain (terms <?> "intermediate terms")) <|> return []
  terms <?> "ending terms"
  return r

program = do
  c <- code
  eof
  return c

parseIxl = parse program

main = do
  c <- getContents
  case parse program "(stdin)" c of
       Left e -> do putStrLn "Error parsing input:"
                    print e
       Right r -> putStr $ unlines $ compile r
