module Ixl.Eval (
  evalIxl
) where

import Ixl.Env
import Ixl.Syntax
import qualified Data.Map as Map
import Control.Monad.State
import Data.Map ((!))

evalIxl :: String -> String -> IO IxlObject
evalIxl source code =
  case parseIxl source code of
       Left e -> return $ IxlString (show e)
       Right ast -> evalIO ast

class AST a where
  eval :: a -> IxlResult
  evalIO :: a -> IO IxlObject
  evalIO node = evalStateT (eval node) baseContext

instance AST ASTExpr where
  eval (NumberNode n) = return $ IxlInt (read n :: Integer)
  eval (StringNode s) = return $ IxlString s
  eval (VariableNode v) = do
    ctx <- get
    return (ctx ! v)

  eval (SubstNode s) = eval s

  eval (LambdaNode (namedArgs, l)) = do
    context <- get
    return $ IxlLambda $ callLambda l context

    where
      callLambda :: ASTProgram -> IxlContext -> [IxlObject] -> IxlResult
      callLambda prg ctx args = do
        liftIO $ evalStateT (eval prg) (setupArgs ctx args)

      setupArgs ctx args = Map.unions [named, it, at, ctx] where
        it = case args of
                  [] -> Map.empty
                  (x:_) -> (Map.singleton "" x)

        at = Map.singleton "@" (IxlList args)
        named = Map.fromList $ zip namedArgs args
        -- TODO: set up .1, .2, etc
        -- numbered = Map.fromList ...

  eval _ = fail "not implemented"

instance AST ASTCommand where
  eval (CommandNode c) = do
    -- TODO: actually call a thing
    vec <- mapM eval c
    let (cmd:args) = vec
    call cmd args

call :: IxlObject -> [IxlObject] -> IxlResult
call (IxlLambda l) args = l args
call (IxlString s) args = do
  ctx <- get
  call (ctx ! s) args

instance AST ASTPipeChain where
  eval (PipeChainNode pc) = do
    ctx <- get
    cmds <- mapM (pipeFragment ctx) pc

    -- restore the original context
    -- TODO: preserve .
    put ctx
    return $ last cmds

    where pipeFragment ctx command = do
            res <- eval command
            put $ Map.insert "" res ctx
            return res

instance AST ASTProgram where
  eval (ProgramNode pr) = do
    chains <- mapM eval pr
    return $ last chains
