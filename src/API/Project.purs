module API.Project (ProjectId(..), Project, getProjects, findProject) where

import Prelude

import Affjax.ResponseFormat as AXRF
import Affjax.Web as AX
import Data.Either (Either)
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Simple.JSON (class ReadForeign)

import Util.Parse (affErr)

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
apiProjectsURL = "http://localhost:8080/projects"

apiProjectURL :: ProjectId -> String
apiProjectURL id = "http://localhost:8080/projects/" <> show id

getProjects :: Aff (Either String (Array Project))
getProjects = affErr <$> AX.get AXRF.string apiProjectsURL

findProject :: ProjectId -> Aff (Either String Project)
findProject id = affErr <$> AX.get AXRF.string (apiProjectURL id)
