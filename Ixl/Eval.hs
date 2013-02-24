module Ixl.Eval (
  evalIxl
) where

import Ixl.Syntax as Syntax
import Ixl.Env
import Ixl.RefMap (Ref, RefMap)
import Data.Map as Map
import Control.Monad.State
import Control.Applicative ((<$>))

evalIxl :: (Evallable a) => a -> IO Value
evalIxl = runInterpIO emptyState . eval

class Evallable a where
  eval :: a -> Interp Value

instance Evallable Syntax.Term where
  eval (Syntax.StringLiteral s) = return (VString s)
  eval (Symbol s) = VSymbol <$> intern s
  eval (Subst p) = eval p

instance Evallable Syntax.Command where
  -- TODO
  eval c = undefined

instance Evallable Syntax.Program where
  -- TODO
  eval p = undefined

intern :: String -> Interp Ref
intern name = do
  state <- get
  case name2id name state of
       Just id -> return id
       Nothing -> do
         id <- nextSymbolRef
         addSymbol name id
         return id
