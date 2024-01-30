module Util.Parse (affErr, affErr_) where

import Prelude

import Affjax.Web as AX
import Data.Either (Either)
import Simple.JSON (class ReadForeign, readJSON)

import Util.Either (mapL)

affErr
  :: forall a
   . ReadForeign a
  => Either AX.Error (AX.Response String)
  -> Either String a
affErr res = mapL show <<< readJSON <<< _.body =<< mapL AX.printError res

affErr_
  :: Either AX.Error Unit
  -> Either String Unit
affErr_ res = mapL AX.printError res
