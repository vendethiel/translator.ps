module Component.ProjectPage (projectPage) where

import Prelude

import API.Project (ProjectId, findProject)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks

-- TODO https://github.com/JordanMartinez/purescript-halogen-hooks-extra

projectPage
  :: forall q o m
   . MonadAff m
   => H.Component q ProjectId o m
projectPage = Hooks.component \_ projectId -> Hooks.do
  mbProject /\ projectStateId <- Hooks.useState Nothing

  Hooks.useLifecycleEffect do
    project <- H.liftAff $ findProject projectId
    Hooks.modify_ projectStateId \_ -> Just project
    pure Nothing

  Hooks.pure do
    case mbProject of
      Nothing -> HH.h1_ [ HH.text $ "Project #" <> show projectId ]
      Just (Left err) -> HH.h1_
        [ HH.text $ "Error loading project #" <> show projectId <> ": " <> err ]
      Just (Right project) -> HH.h1_ [ HH.text project.name ]
