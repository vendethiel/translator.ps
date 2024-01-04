module API.Project (ProjectId(..), Project, getProjects, findProject) where

import Prelude

import Affjax.ResponseFormat as AXRF
import Affjax.Web as AX
import Data.Array (find)
import Data.Either (Either)
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Simple.JSON (class ReadForeign, readJSON)

import Util.Either (mapL, fromEitherMaybe)

newtype ProjectId = ProjectId Int
derive instance Newtype ProjectId _
derive newtype instance Eq ProjectId
derive newtype instance Ord ProjectId
derive newtype instance Show ProjectId
derive newtype instance ReadForeign ProjectId

type Project =
  { id :: ProjectId
  , name :: String }

apiProjectsURL :: String
apiProjectsURL = "https://gist.githubusercontent.com/vendethiel/f763232225ce3109a2202e2ae0e261a6/raw/0adf9f40f22d838253d416205e610b65e1b948d7/projects.json"

getProjects :: Aff (Either String (Array Project))
getProjects = do
  res <- AX.get AXRF.string apiProjectsURL
  pure $ mapL show <<< readJSON <<< _.body =<< mapL AX.printError res

findProject :: ProjectId -> Aff (Either String Project)
findProject id = do
  projects <- getProjects
  pure $ fromEitherMaybe "Not Found" $ map (find $ \p -> p.id == id) projects
