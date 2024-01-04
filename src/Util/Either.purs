module Util.Either (fromEitherMaybe, mapL) where

import Prelude (identity)

import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

mapL :: forall a b c. (a -> b) -> Either a c -> Either b c
mapL f = bimap f identity

fromEitherMaybe :: forall l r. l -> Either l (Maybe r) -> Either l r
fromEitherMaybe _ (Right (Just x)) = Right x
fromEitherMaybe l (Right Nothing) = Left l
fromEitherMaybe _ (Left e) = Left e
