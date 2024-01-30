module API.Task
  (TaskId(..)
  , Task
  , Translations(..)
  , LangCode(..)
  , getProjectTasks
  , findProjectTask
  , translateTaskKey
  , toUnfoldable
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Affjax.RequestBody as AXRB
import Affjax.ResponseFormat as AXRF
import Affjax.Web as AX
import Data.Either (Either)
import Data.Map as Map
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Data.Unfoldable (class Unfoldable)
import Effect.Aff (Aff)
import Foreign.Object (Object)
import Simple.JSON (class ReadForeign, readImpl)

import API.Project (ProjectId)
import Util.Parse (affErr, affErr_)
import Util.Map (foldWithIndex)

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
derive newtype instance ReadForeign LangCode
instance Show LangCode where
  show (LangCode lc) = lc

newtype Translations = Translations (Map.Map LangCode String)
derive instance Newtype Translations _

toUnfoldable :: forall f. Unfoldable f => Translations -> f (Tuple LangCode String)
toUnfoldable (Translations m) = Map.toUnfoldable m

instance ReadForeign Translations where
  readImpl json = do
    (o :: Object String) <- readImpl json
    pure $ Translations $ foldWithIndex LangCode o

type Task =
  { id :: TaskId
  , projectId :: ProjectId
  , name :: String
  , translations :: Translations }

apiTasksURL :: ProjectId -> String
apiTasksURL projectId = "http://localhost:8080/projects/" <> show projectId <> "/tasks"

apiTaskURL :: ProjectId -> TaskId -> String
apiTaskURL projectId taskId = "http://localhost:8080/projects/" <> show projectId <> "/tasks/" <> show taskId

translateUrl :: ProjectId -> TaskId -> LangCode -> String
translateUrl projectId taskId lang = "http://localhost:8080/projects/" <> show projectId <> "/tasks/" <> show taskId <> "/lang/" <> show lang

getProjectTasks :: ProjectId -> Aff (Either String (Array Task))
getProjectTasks projectId = affErr <$> AX.get AXRF.string (apiTasksURL projectId)

findProjectTask :: ProjectId -> TaskId -> Aff (Either String Task)
findProjectTask projectId taskId = affErr <$> AX.get AXRF.string (apiTaskURL projectId taskId)

translateTaskKey :: ProjectId -> TaskId -> LangCode -> String -> Aff (Either String Unit)
translateTaskKey projectId taskId lang value = affErr_ <$> AX.post_ (translateUrl projectId taskId lang) (Just $ AXRB.string value)
