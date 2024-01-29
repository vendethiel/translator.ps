module Component.TaskItem (taskItem) where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks
import Type.Proxy (Proxy(..))

import API.Task (Task, toUnfoldable, findProjectTask)
import Component.TranslationInput as TranslationInput

_translationInput = Proxy :: Proxy "translationInput"

header :: forall a b. HH.HTML a b
header = HH.thead_
  [ HH.tr_
    [ HH.th_ [ HH.text "Language" ]
    , HH.th_ [ HH.text "Translation" ]  ] ]

taskItem
  :: forall q m
   . MonadAff m
  => H.Component q Task Unit m
taskItem = Hooks.component \_ initialTask -> Hooks.do
  task /\ taskStateId <- Hooks.useState initialTask
  mbEdit /\ editStateId <- Hooks.useState Nothing
  mbErr /\ errStateId <- Hooks.useState Nothing

  let
    line = showLine editStateId

    reloadTask _ = reload task.projectId task.id taskStateId errStateId editStateId

    translate (TranslationInput.Translate _lang _value) = do
      -- TODO send new
      Hooks.put editStateId Nothing
      reloadTask unit

    edit s = HH.slot _translationInput unit TranslationInput.component s translate

  Hooks.pure do
    HH.div [ HE.onClick \_ -> reloadTask unit ] $ join
      [ [ HH.text $ task.name ]
      , edit <$> fromMaybe mbEdit
      , err <$> fromMaybe mbErr
      , [ HH.table_
          [ header
          , HH.tbody_ $ map line $ toUnfoldable task.translations ] ] ]
  where
  showLine s l@(lang /\ value) = HH.tr
    [ HE.onClick \_ -> Hooks.modify_ s \_ -> Just l ]
    [ HH.td_ [ HH.text $ show lang ]
    , HH.td_ [ HH.text value ] ]

  err error = HH.text $ "Error: " <> error

  reload projectId taskId taskStateId errStateId editStateId = do
    resp <- H.liftAff $ findProjectTask projectId taskId
    case resp of
      Left error -> Hooks.put errStateId $ Just error
      Right task -> do -- reset state
        Hooks.put errStateId Nothing
        Hooks.put editStateId Nothing
        Hooks.put taskStateId task
