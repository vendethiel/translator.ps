module Component.TaskItem (taskItem) where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks

import API.Task (Task)

taskItem
  :: forall q o m
   . MonadAff m
  => H.Component q Task o m
taskItem = Hooks.component \_ task -> Hooks.do
  Hooks.pure do
    HH.h3_ [ HH.text $ task.name ]
