{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ixl.Env (
  InterpState,
  emptyState,
  addSymbol,
  name2id,
  id2name,
  nextHeapRef,
  nextSymbolRef,

  Object,
  emptyObject,

  Value(..),
  typeOfValue,
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
import Control.Applicative ((<$>))

data InterpState = InterpState {
  st'internTable :: Map.Map String Ref,
  st'reverseTable :: RefMap String,
  st'heap :: RefMap Value,
  st'stack :: [Frame]
} deriving(Show)


emptyState :: InterpState
emptyState = InterpState {
  st'internTable = Map.empty,
  st'reverseTable = RefMap.empty,
  st'heap = RefMap.empty,
  st'stack = []
}

addSymbol :: String -> Ref -> Interp ()
addSymbol str id = modify $ \state ->
  let forward = st'internTable state
      reverse = st'reverseTable state
  in state { st'internTable = Map.insert str id forward,
             st'reverseTable = RefMap.insert id str reverse }

nextSymbolRef :: Interp Ref
nextSymbolRef = Ref . Map.size . st'internTable <$> get

nextHeapRef :: Interp Ref
nextHeapRef = RefMap.next .  st'heap <$> get

name2id :: String -> InterpState -> Maybe Ref
name2id s = Map.lookup s . st'internTable

id2name :: Ref -> Interp (Maybe String)
id2name r = RefMap.lookup r . st'reverseTable <$> get

dereference :: Ref -> Interp Value
dereference r = fromJust . RefMap.lookup r . st'heap <$> get

makeBox :: Type -> Value -> Interp Value
makeBox type_ val = do
  id <- nextHeapRef
  modify $ \state -> state {
             st'heap = RefMap.insert id val (st'heap state)
           }
  return $ VBox (id, type_)

data Object = Object {
  o'type :: Type,
  o'id :: Ref,
  o'vars :: RefMap Value,
  o'native :: Maybe Dynamic
} deriving(Show)

-- immutable Value objects
data Value = VObject Object
           | VSymbol Ref
           | VBox (Ref, Type)
           | VString String
           | VInt Int
           | VFloat Float
           | VNative Dynamic
           | VBlock Block
           deriving(Show)

data Type = UnitType String
          | BlockType ArgSpec Type
          | RefType Type
            deriving(Show, Eq)

data Block = Block {
  block'body :: (), -- TODO
  block'argSpec :: ArgSpec
} deriving(Show)

objectType = UnitType "object"
typeType = UnitType "type"
symbolType = UnitType "symbol"
boxType = UnitType "box"
stringType = UnitType "string"
intType = UnitType "int"
floatType = UnitType "float"
nativeType = UnitType "native"

typeOfValue :: Value -> Type
typeOfValue (VObject x) = o'type x
typeOfValue (VSymbol _) = symbolType
typeOfValue (VBox (_, t)) = RefType t
typeOfValue (VString _) = stringType
typeOfValue (VInt _) = intType
typeOfValue (VFloat _) = floatType
typeOfValue (VNative _) = nativeType

emptyObject :: Object
emptyObject = Object {
  o'type = objectType,
  o'id = RefMap.nullRef, -- NB: this is invalid, and must be registered!
  o'vars = RefMap.empty,
  o'native = Nothing
}

data Frame = Frame {
  fr'block :: Object,
  fr'locals :: RefMap Value,
  fr'target :: Int
} deriving(Show)

type Env = [Frame]

data ArgSlot = FlagSlot String
             | VarSlot (String, Maybe Type)
             deriving (Eq, Show)
type ArgSpec = [ArgSlot]

{-- The Interp Monad --}

newtype Interp a = Interp {
  runInterp :: ContT Value (StateT InterpState IO) a
} deriving(Monad,
           Functor,
           MonadIO,
           MonadState InterpState,
           MonadCont
          )

runInterpIO :: InterpState -> Interp Value -> IO Value
runInterpIO state action =
  (`evalStateT` state) $ (`runContT` return) $ runInterp action

runInterpSandboxed :: Interp Value -> IO Value
runInterpSandboxed = runInterpIO emptyState
