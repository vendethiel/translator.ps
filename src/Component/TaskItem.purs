module Component.TaskItem (taskItem) where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks

import API.Task (Task, toUnfoldable, findProjectTask)

header :: forall a b. HH.HTML a b
header = HH.thead_
  [ HH.tr_
    [ HH.th_ [ HH.text "Language" ]
    , HH.th_ [ HH.text "Translation" ]  ] ]

taskItem
  :: forall q o m
   . MonadAff m
  => H.Component q Task o m
taskItem = Hooks.component \_ initialTask -> Hooks.do
  task /\ taskStateId <- Hooks.useState initialTask
  mbEdit /\ editStateId <- Hooks.useState Nothing
  mbErr /\ errStateId <- Hooks.useState Nothing

  let
    line = showLine editStateId
    reloadTask _ = reload task.projectId task.id taskStateId errStateId

  Hooks.pure do
    HH.div [ HE.onClick reloadTask ] $ join
      [ [ HH.text $ task.name ]
      , edit mbEdit
      , err mbErr
      , [ HH.table_
          [ header
          , HH.tbody_ $ map line $ toUnfoldable task.translations ] ] ]
  where
  showLine s l@(lang /\ value) = HH.tr
    [ HE.onClick \_ -> Hooks.modify_ s \_ -> Just l ]
    [ HH.td_ [ HH.text $ show lang ]
    , HH.td_ [ HH.text value ] ]

  edit Nothing = []
  edit (Just (lang /\ value)) = [ HH.div_
    [ HH.text $ "Lang: " <> show lang
    , HH.input [ HP.value value ] ] ]

  err Nothing = []
  err (Just error) = [ HH.text $ "Error: " <> error ]

  reload projectId taskId taskStateId errStateId = do
    resp <- H.liftAff $ findProjectTask projectId taskId
    case resp of
      Left error -> Hooks.put errStateId $ Just error
      Right task -> Hooks.put taskStateId task
