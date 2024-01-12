module API.Task
  (TaskId(..)
  , Task
  , Translations(..)
  , LangCode(..)
  , getProjectTasks
  ) where

import Prelude

import Affjax.ResponseFormat as AXRF
import Affjax.Web as AX
import Data.Either (Either)
import Data.Map as Map
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Foreign.Object (Object)
import Simple.JSON (class ReadForeign, readImpl)

import API.Project (ProjectId)
import Util.Map (foldWithIndex)
import Util.Parse (affErr)

newtype TaskId = TaskId Int
derive instance Newtype TaskId _
derive newtype instance Eq TaskId
derive newtype instance Ord TaskId
derive newtype instance Show TaskId
derive newtype instance ReadForeign TaskId

newtype LangCode = LangCode String
derive instance Newtype LangCode _
derive newtype instance Eq LangCode
derive newtype instance Ord LangCode
derive newtype instance Show LangCode
derive newtype instance ReadForeign LangCode

newtype Translations = Translations (Map.Map LangCode String)
derive instance Newtype Translations _

instance ReadForeign Translations where
  readImpl json = do
    (o :: Object String) <- readImpl json
    pure $ Translations $ foldWithIndex LangCode o

type Task =
  { id :: TaskId
  , projectId :: ProjectId
  , key :: String
  , translations :: Translations }

apiTasksURL :: ProjectId -> String
apiTasksURL projectId = "" <> show projectId

getProjectTasks :: ProjectId -> Aff (Either String (Array Task))
getProjectTasks projectId = affErr <$> AX.get AXRF.string (apiTasksURL projectId)
