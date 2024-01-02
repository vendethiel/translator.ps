module ProjectList (projectList) where

import Prelude
import Affjax.Web as AX
import Affjax.ResponseFormat as AXRF
import Data.Array (length)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH

apiProjectsURL :: String
apiProjectsURL = "https://gist.githubusercontent.com/vendethiel/f763232225ce3109a2202e2ae0e261a6/raw/0adf9f40f22d838253d416205e610b65e1b948d7/projects.json"

data Project = Project
  { id :: Int
  , name :: String }

type Input = Unit
type Output = Unit
data State = Loading | Data (Array Project)
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
      (Data xs) -> HH.text $ "Got " <> show (length xs) <> " project(s)" ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    _ <- H.liftAff $ AX.get AXRF.string apiProjectsURL
    H.modify_ \_ -> Data []
