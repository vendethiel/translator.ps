module Util.Map (foldWithIndex) where

import Prelude

import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map as Map
import Foreign.Object (Object)

foldWithIndex :: forall k v. Ord k => (String -> k) -> Object v -> Map.Map k v
foldWithIndex f = foldlWithIndex (\k m v -> Map.insert (f k) v m) Map.empty

