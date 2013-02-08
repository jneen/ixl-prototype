{-# LANGUAGE FlexibleInstances #-}

module Ixl.Util (
  ShowIO,
  showIO,
  print',
) where

import Data.Dynamic (Dynamic)

class ShowIO a where
  showIO :: a -> IO String
  print' :: a -> IO ()
  print' x = showIO x >>= putStrLn

instance ShowIO Char    where showIO = return . show
instance ShowIO Integer where showIO = return . show
instance ShowIO Dynamic where showIO = return . show

instance (ShowIO a) => ShowIO [a] where
  showIO xs = mapM showIO xs >>= (return . show)

instance (ShowIO a, ShowIO b) => ShowIO (a, b) where
  showIO (x, y) = do
    sx <- showIO x
    sy <- showIO y
    return $ show (sx, sy)
