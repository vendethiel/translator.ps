module Component.ProjectList (projectList) where

import Prelude

import Data.Array (length)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH

import API.Project (Project, getProjects)

type Input = Unit
type Output = Unit
data State = Loading | APIError String | Data (Array Project)
type Query = Unit
data Action = Initialize

projectList :: forall query m. MonadAff m => H.Component query Input Output m
projectList =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize }}

initialState :: Input -> State
initialState _ = Loading

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ case state of
      Loading -> HH.text "Loading..."
      APIError e -> HH.text $ "API Error while fetching projects: " <> e
      Data xs -> HH.text $ "Got " <> show (length xs) <> " project(s)" ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    projects <- H.liftAff getProjects
    H.modify_ \_ -> either APIError Data projects
