module Musicology.Internal.GatedQueue
  ( GatedQueue
  , insert
  , insertWith
  , release
  , Data.Map.null
  , union
  ) where

import Data.Map

type GatedQueue k v = Map k v

release :: Ord k => GatedQueue k v -> k -> (GatedQueue k v, [v])
release q gate = (q', elems rels)
  where (rels, q') = spanAntitone (<=gate) q
