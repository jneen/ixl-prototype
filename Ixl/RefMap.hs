module Ixl.RefMap where

import qualified Data.IntMap as IM

newtype Ref = Ref { r'int :: Int } deriving(Show, Eq, Ord)
newtype RefMap a = RefMap { rm'map :: IM.IntMap a } deriving(Show)

ref = Ref

nullRef = Ref (-1)

lookup :: Ref -> RefMap a -> Maybe a
lookup ref rmap = IM.lookup (r'int ref) (rm'map rmap)

empty = RefMap IM.empty

insert :: Ref -> a -> RefMap a -> RefMap a
insert k v = RefMap . IM.insert (r'int k) v . rm'map

next :: RefMap a -> Ref
next = Ref . IM.size . rm'map
