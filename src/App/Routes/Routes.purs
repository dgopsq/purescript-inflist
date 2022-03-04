module App.Routes where

import Prelude
import App.State.Todo (TodoId)
import Data.Foldable as Foldable
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import React.Basic (ReactContext)
import Routing.Match (Match)
import Routing.Match as Match
import Routing.PushState (PushStateInterface)

-- | This is the data structure that describes the
-- | application's routes.
data AppRoute
  = RootTodos
  | ChildrenTodos TodoId

derive instance genericAppRoute :: Generic AppRoute _

derive instance eqAppRoute :: Eq AppRoute

instance showAppRoute :: Show AppRoute where
  show = genericShow

-- | The parser used to match a path
-- | with a route from the `AppRoute` sum type.
mkAppRoute :: Maybe String -> Match (Maybe AppRoute)
mkAppRoute maybePathPrefix =
  Foldable.oneOf
    [ Just <$> routes
    , pure Nothing
    ]
  where
  routes =
    ( fromMaybe Match.root
        $ map
            (\path -> Match.root *> Match.lit path)
            maybePathPrefix
    )
      *> Foldable.oneOf
          [ ChildrenTodos <$> Match.str
          , pure RootTodos
          ]
      <* Match.end

-- | The type describing a React context that contains
-- | the application's routes utilities.
type RouterContext
  = ReactContext (Maybe RouterContextValue)

-- | The data structure containing all the routing utilities.
-- | This Record contains the current route and the 
-- | navigation interface used to manage the browser native
-- | `PushState` navigation.
type RouterContextValue
  = { route :: Maybe AppRoute
    , nav :: PushStateInterface
    }

-- | The type used in the `Reader` monad to
-- | describe all the dependencies related to routing.
type Router
  = { routerContext :: RouterContext }
