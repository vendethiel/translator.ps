module Util.Either (fromEitherMaybe, mapL) where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either, note)
import Data.Maybe (Maybe)

mapL :: forall a b c. (a -> b) -> Either a c -> Either b c
mapL f = bimap f identity

-- unused
fromEitherMaybe :: forall l r. l -> Either l (Maybe r) -> Either l r
fromEitherMaybe = (=<<) <<< note
