module Component.TaskItem (taskItem) where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks

import API.Task (Task, toUnfoldable)

header :: forall a b. HH.HTML a b
header = HH.thead_
  [ HH.tr_
    [ HH.th_ [ HH.text "Language" ]
    , HH.th_ [ HH.text "Translation" ]  ] ]

taskItem
  :: forall q o m
   . MonadAff m
  => H.Component q Task o m
taskItem = Hooks.component \_ task -> Hooks.do
  Hooks.pure do
    HH.div_
      [ HH.text $ task.name
      , HH.table_
        [ header
        , HH.tbody_ $ map line $ toUnfoldable task.translations ] ]
  where
  line (lang /\ value) = HH.tr_
    [ HH.td_ [ HH.text $ show lang ]
    , HH.td_ [ HH.text value ] ]
