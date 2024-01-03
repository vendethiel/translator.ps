module Component.Root (root) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Router.Class (class MonadRouter)
import Halogen.Router.UseRouter (useRouter)
import Type.Proxy (Proxy(..))

import Route (AppRoute(..))
import Component.ProjectList (projectList)

_projectList = Proxy :: Proxy "projectList"

root :: forall q i o m
      . MonadRouter AppRoute m
     => MonadAff m
     => H.Component q i o m
root = Hooks.component \_ _ -> Hooks.do
  current /\ _ <- useRouter

  Hooks.pure do
    HH.div [ HP.id "app" ]
      [ HH.text "Page Title Here"
      , routerView current
      ]

  where
  routerView :: Maybe AppRoute -> HH.HTML _ _
  routerView = case _ of
    Nothing -> HH.text "404"
    Just ProjectListPage -> HH.slot_ _projectList unit projectList unit
    Just _ -> HH.text "TODO"
