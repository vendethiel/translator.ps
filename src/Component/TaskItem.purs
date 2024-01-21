module Component.TaskItem (taskItem) where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
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
  mbEdit /\ editStateId <- Hooks.useState Nothing

  Hooks.pure do
    HH.div_ $ join
      [ [ HH.text $ task.name ]
      , edit mbEdit
      , [ HH.table_
          [ header
          , HH.tbody_ $ map (line editStateId) $ toUnfoldable task.translations ] ] ]
  where
  line s l@(lang /\ value) = HH.tr
    [ HE.onClick \_ -> Hooks.modify_ s \_ -> Just l ]
    [ HH.td_ [ HH.text $ show lang ]
    , HH.td_ [ HH.text value ] ]

  edit Nothing = []
  edit (Just (lang /\ value)) = [ HH.div_
    [ HH.text $ "Lang: " <> show lang
    , HH.input [ HP.value value ] ] ]
