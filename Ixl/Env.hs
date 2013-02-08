{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ixl.Env (
  InterpState,
  emptyState,
  addSymbol,
  getSymbolName,
  getSymbolRef,
  nextRef,

  Object,
  emptyObject,

  Value(..),
  Type,
  typeType,
  objectType,

  Interp(..),
  runInterpIO,
) where

import qualified Ixl.RefMap as RefMap
import qualified Ixl.Syntax as Syntax
import Ixl.RefMap (RefMap, Ref(..))

import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Data.Dynamic

import Control.Monad.Cont
import Control.Monad.State

data InterpState = InterpState {
  st'internTable :: Map.Map String Ref,
  st'reverseTable :: RefMap String,
  st'heap :: RefMap Object,
  st'stack :: [Frame]
} deriving(Show)


emptyState :: InterpState
emptyState = InterpState {
  st'internTable = Map.empty,
  st'reverseTable = RefMap.empty,
  st'heap = RefMap.empty,
  st'stack = []
}

addSymbol :: String -> Ref -> InterpState -> InterpState
addSymbol str id state =
  let forward = st'internTable state
      reverse = st'reverseTable state
  in state { st'internTable = Map.insert str id forward,
             st'reverseTable = RefMap.insert id str reverse }

nextRef :: InterpState -> Ref
nextRef = Ref . Map.size . st'internTable

getSymbolRef :: String -> InterpState -> Maybe Ref
getSymbolRef s = Map.lookup s . st'internTable

getSymbolName :: Ref -> InterpState -> Maybe String
getSymbolName r = RefMap.lookup r . st'reverseTable

data Object = Object {
  o'type :: Type,
  o'id :: Ref,
  o'vars :: RefMap Ref,
  o'native :: Maybe Dynamic
} deriving(Show)

-- object references and immutable types
data Value = VObject Ref
           | VSymbol Ref
           | VString String
           | VInt Int
           | VFloat Float
           -- | Block (RefMap Variable) Syntax.Program
           -- deriving(Show)

data Type = UnitType String | BlockType [Either String Type] Type
            deriving(Show, Eq)

objectType = UnitType "object"
typeType = UnitType "type"

emptyObject :: Object
emptyObject = Object {
  o'type = objectType,
  o'id = RefMap.nullRef, -- NB: this is invalid, and must be registered!
  o'vars = RefMap.empty,
  o'native = Nothing
}

data Frame = Frame {
  fr'block :: Object,
  fr'locals :: RefMap Ref,
  fr'target :: Int
} deriving(Show)

type Env = [Frame]

{-- The Interp Monad --}

type Interp = InterpT (ContT Value (StateT InterpState IO))
newtype InterpT m a = InterpT { runInterpT :: m a }

instance MonadTrans InterpT where
  lift = InterpT

instance Monad m => Monad (InterpT m) where
  return = InterpT . return
  x >>= f = InterpT $ (runInterpT x) >>= (runInterpT . f)

instance (MonadIO m) => MonadIO (InterpT m) where
  liftIO = InterpT . liftIO

instance Functor m => Functor (InterpT m) where
  fmap f (InterpT a) = InterpT (fmap f a)

-- provides `modify`
instance MonadState InterpState Interp where
  -- put :: InterpState -> Interp ()
  -- put (inner) :: InterpState -> StateT InterpState IO ()
  put = lift . lift . put
  -- get :: Interp InterpState
  -- get (inner) :: StateT InterpState IO InterpState
  get = lift $ lift get

instance MonadCont Interp where
  -- callCC :: ((a -> Interp b) -> Interp a) -> Interp a
  -- callCC (inner) ::
  --           ((a -> ContT Value m b) -> ContT Value m a) -> ContT Value m a
  -- f :: (a -> Interp b) -> Interp a
  -- arg to callCC (inner) :: (a -> ContT Value m b) -> ContT Value m a
  callCC f = lift $ callCC cc
    where
      cc next = runInterpT $ f (lift . next)

runInterpIO :: InterpState -> Interp Value -> IO Value
runInterpIO state evalue =
  (`evalStateT` state) $ (`runContT` return) $ runInterpT evalue
