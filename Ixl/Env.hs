{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Ixl.Env (
  bindingsFromList,
  makeNative,
  Type(..),
  Object(..),
  Env(..),
  showIO,
) where

import qualified Data.Map as Map
import Ixl.Util (showIO, ShowIO)
import Data.Dynamic
import Data.IORef

{---- Interpreting ----}

type Bindings a = IORef (Map.Map String (IORef a))

data Type = Type {
  type_name :: String,
  type_mixins :: [Type]
}

data Object = Object {
  obj_type :: Type,
  obj_bindings :: Bindings Object,
  obj_native :: Dynamic
}

instance (ShowIO a) => ShowIO (Bindings a) where
  showIO bindings = do
    list <- bindingsToList bindings
    repr <- showIO list
    return $ "bindingsFromList " ++ repr

data Env = Env {
  env_target :: Maybe Object,
  env_parent :: Maybe Env,
  env_bindings :: Bindings Object,
  env_types :: Bindings Type
}

bindingsFromList :: [(String, a)] -> IO (Bindings a)
bindingsFromList list = do
  refs <- mapM makeBinding list
  newIORef $ Map.fromList refs
  where
    makeBinding :: (String, a) -> IO (String, IORef a)
    makeBinding (s, o) = do
      ref <- newIORef o
      return (s, ref)

bindingsToList :: Bindings a -> IO [(String, a)]
bindingsToList bindings = do
  bindingMap <- readIORef bindings
  let refsList = Map.toList bindingMap
  mapM unpack refsList
  where
    unpack :: (String, IORef a) -> IO (String, a)
    unpack (s, r) = do { x <- readIORef r; return (s, x) }

get :: Bindings a -> String -> IO (Maybe a)
get bindings name = do
  bindingMap <- readIORef bindings
  case Map.lookup name bindingMap of
       Just ref -> fmap Just $ readIORef ref
       _ -> return Nothing

set :: Bindings a -> String -> a -> IO ()
set bindings name val = do
  bindingMap <- readIORef bindings
  case Map.lookup name bindingMap of
       Just ref -> writeIORef ref val
       Nothing  -> do
         ref <- newIORef val
         let newMap = Map.insert name ref bindingMap
         writeIORef bindings newMap

makeNative :: (Typeable a) => Type -> a -> IO Object
makeNative type_ native = do
  bindings <- bindingsFromList []
  return $ Object { obj_type = type_,
                    obj_bindings = bindings,
                    obj_native = toDyn native
                  }

getNative :: (Typeable a) => Type -> Object -> Maybe a
getNative type_ obj
  | type_name (obj_type obj) /= type_name type_ = Nothing
  | otherwise = (fromDynamic . obj_native) obj

-- baseContext = Map.fromList [ ("add", IxlLambda ixlAdd),
--                              ("mul", IxlLambda ixlMul),
--                              ("empty", IxlEmpty),
--                              (":", IxlLambda ixlColon),
--                              ("puts", IxlLambda ixlPuts) ]
-- 
-- ixlAdd :: [IxlObject] -> IxlResult
-- ixlAdd args = return $ foldl1 plus args where
--   plus (IxlInt n) (IxlInt m) = IxlInt (n + m)
-- 
-- ixlMul :: [IxlObject] -> IxlResult
-- ixlMul args = return $ foldl1 times args where
--   times (IxlInt n) (IxlInt m) = IxlInt (n * m)
-- 
-- ixlColon :: [IxlObject] -> IxlResult
-- ixlColon args = return $ last args
-- 
-- ixlPuts :: [IxlObject] -> IxlResult
-- ixlPuts args = do
--   liftIO $ puts arg
--   return arg
-- 
--   where arg = head args
--         puts (IxlString s) = putStrLn s
--         puts (IxlInt n)    = putStrLn $ show n
