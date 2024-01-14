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
import Data.Traversable (for)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Foreign (ForeignError(..), fail)
import Simple.JSON (class ReadForeign, readImpl)
import API.Project (ProjectId)
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
    (o :: Array (Array String)) <- readImpl json
    kvs <- for o $ case _ of
      [k, v] -> pure $ LangCode k /\ v
      _ -> fail $ TypeMismatch "tuple of 2 strings" "an array of more or less than 2 elements"
    pure $ Translations $ Map.fromFoldable kvs

type Task =
  { id :: TaskId
  , projectId :: ProjectId
  , name :: String
  , translations :: Translations }

apiTasksURL :: ProjectId -> String
apiTasksURL projectId = "http://localhost:8080/projects/" <> show projectId <> "/tasks"

getProjectTasks :: ProjectId -> Aff (Either String (Array Task))
getProjectTasks projectId = affErr <$> AX.get AXRF.string (apiTasksURL projectId)
