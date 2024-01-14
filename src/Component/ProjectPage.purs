module Component.ProjectPage (projectPage) where

import Prelude

import Control.Apply (lift2)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks

import API.Project (ProjectId, findProject)
import API.Task (getProjectTasks)
-- TODO https://github.com/JordanMartinez/purescript-halogen-hooks-extra

projectPage
  :: forall q o m
   . MonadAff m
   => H.Component q ProjectId o m
projectPage = Hooks.component \_ projectId -> Hooks.do
  mbData /\ dataStateId <- Hooks.useState Nothing

  Hooks.useLifecycleEffect do
    projectAndTasks <- H.liftAff $ lift2 (/\) <$> findProject projectId <*> getProjectTasks projectId
    Hooks.modify_ dataStateId \_ -> Just projectAndTasks
    pure Nothing

  Hooks.pure do
    case mbData of
      Nothing -> HH.h1_ [ HH.text $ "Loading Project #" <> show projectId ]
      Just (Left err) -> HH.h1_
        [ HH.text $ "Error loading project #" <> show projectId <> ": " <> err ]
      Just (Right (project /\ _)) -> HH.h1_ [ HH.text project.name ]
