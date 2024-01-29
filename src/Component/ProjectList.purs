module Component.ProjectList (projectList) where

import Prelude

import API.Project (Project, ProjectId, getProjects)
import Component.ProjectLabel (projectLabel)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Router.Class (class MonadRouter)
import Route (AppRoute)
import Type.Proxy (Proxy(..))

type Input = Unit
type Output = Unit
data State = Loading | APIError String | Data (Array Project)
data Action = Initialize
type Slots = (projectLabel :: forall query. H.Slot query Void ProjectId)

_projectLabel = Proxy :: Proxy "projectLabel"

projectList
  :: forall query m
   . MonadRouter AppRoute m
  => MonadAff m
  => H.Component query Input Output m
projectList =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize }}

initialState :: Input -> State
initialState _ = Loading

render :: forall m. MonadRouter AppRoute m => State -> H.ComponentHTML Action Slots m
render state =
  HH.div_ $
    case state of
      Loading -> [ HH.text "Loading..." ]
      APIError e -> [ HH.text $ "API Error while fetching projects: " <> e ]
      Data xs ->
        [ HH.ul_ $ flip map xs \p ->
          HH.li_ [ HH.slot_ _projectLabel p.id projectLabel p ] ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action Slots Output m Unit
handleAction = case _ of
  Initialize -> do
    projects <- H.liftAff getProjects
    H.modify_ \_ -> either APIError Data projects
