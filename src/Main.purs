module Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen.Router.Trans.PushState (mkRouter, runRouterT)

import Route (route)
import Component.Root (root)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  router <- liftEffect $ mkRouter route
  let rootComponent = H.hoist (runRouterT router) root
  runUI rootComponent {} body
