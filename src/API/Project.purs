module API.Project (ProjectId(..), Project, getProjects) where

import Prelude
import Affjax.Web as AX
import Affjax.ResponseFormat as AXRF
import Effect.Aff (Aff)
import Data.Either (Either)
import Simple.JSON (readJSON)
import Data.Bifunctor (bimap)
import Data.Newtype (class Newtype)

type Project =
  { id :: Int
  , name :: String }

newtype ProjectId = ProjectId Int
derive instance Eq ProjectId
derive instance Newtype ProjectId _

apiProjectsURL :: String
apiProjectsURL = "https://gist.githubusercontent.com/vendethiel/f763232225ce3109a2202e2ae0e261a6/raw/0adf9f40f22d838253d416205e610b65e1b948d7/projects.json"

-- TODO purescript-errors or a Util.Either module
mapL :: forall a b c. (a -> b) -> Either a c -> Either b c
mapL f = bimap f identity

getProjects :: Aff (Either String (Array Project))
getProjects = do
  res <- AX.get AXRF.string apiProjectsURL
  pure $ mapL show <<< readJSON <<< _.body =<< mapL AX.printError res
