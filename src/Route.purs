module Route (route, AppRoute(..)) where

import Prelude (($), class Eq)
import API.Project (ProjectId)
import Data.Generic.Rep (class Generic)
import Data.Lens.Iso.Newtype (_Newtype)
import Routing.Duplex (RouteDuplex', root, segment, int)
import Routing.Duplex.Generic (sum, noArgs)
import Routing.Duplex.Generic.Syntax ((/))

data AppRoute
  = ProjectListPage
  | ProjectPage ProjectId

projectId :: RouteDuplex' ProjectId
projectId = _Newtype (int segment)

derive instance Eq AppRoute
derive instance Generic AppRoute _

route :: RouteDuplex' AppRoute
route = root $ sum
  { "ProjectListPage": noArgs -- TODO path "projects"?
  , "ProjectPage": "projects" / projectId
  }
