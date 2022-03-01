module App.Routes where

import Prelude
import App.State.Todo (TodoId)
import Data.Foldable as Foldable
import Data.Maybe (Maybe(..))
import React.Basic (ReactContext)
import Routing.Match (Match)
import Routing.Match as Match
import Routing.PushState (PushStateInterface)

-- | This is the data structure that describes the
-- | application's routes.
data AppRoute
  = RootTodos
  | ChildrenTodos TodoId

-- | The parser used to match a path
-- | with a route from the `AppRoute` sum type.
appRoute :: Match (Maybe AppRoute)
appRoute =
  Foldable.oneOf
    [ Just <$> routes
    , pure Nothing
    ]
  where
  routes =
    Match.root
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
