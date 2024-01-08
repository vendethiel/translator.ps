module Util.Parse (affErr) where

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
