module Component.ProjectLabel (projectLabel) where

import Prelude

import API.Project (Project)
import Data.Tuple.Nested ((/\))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks
import Halogen.Router.Class (class MonadRouter)
import Halogen.Router.UseRouter (useRouter)
import Route (AppRoute(..))
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent as ME

projectLabel
  :: forall q o m
   . MonadRouter AppRoute m
   => H.Component q Project o m
projectLabel = Hooks.component \_ project -> Hooks.do
  _ /\ { navigate } <- useRouter

  Hooks.pure do
    HH.a
      [ HE.onClick \e -> do
          H.liftEffect $ preventDefault $ ME.toEvent e
          navigate (ProjectPage project.id) ]
      [ HH.text project.name ]