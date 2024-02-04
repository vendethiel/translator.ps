module Component.TranslationInput (component, Output(..)) where

import Prelude

import Effect.Class (class MonadEffect)
import Data.Tuple.Nested ((/\), type (/\))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Web.Event.Event (preventDefault)

import API.Task (LangCode)

data Output = Translate LangCode String
type Tokens q s = Hooks.ComponentTokens q s Output

component
  :: forall q m
   . MonadEffect m
  => H.Component q (LangCode /\ String) Output m
component = Hooks.component \({ outputToken } :: Tokens q _) (lang /\ initVal) -> Hooks.do
  value /\ valueStateId <- Hooks.useState initVal

  let
    submit e = do
      H.liftEffect $ preventDefault e
      Hooks.raise outputToken $ Translate lang value

  Hooks.pure do
    HH.div_
      [ HH.text $ "Lang: " <> show lang
      , HH.form
        [ HE.onSubmit submit ]
        [ HH.input
          [ HE.onValueInput $ Hooks.put valueStateId
          , HP.value value ] ] ]
