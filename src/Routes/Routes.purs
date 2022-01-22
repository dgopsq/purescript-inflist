module Routes where

import Prelude
import Data.Foldable as Foldable
import Data.Maybe (Maybe(..))
import React.Basic (ReactContext)
import Routing.Match (Match)
import Routing.Match as Match
import Routing.PushState (PushStateInterface)

-- | The App routes structure.
data AppRoute
  = Home
  | About

-- | The App routes parser.
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
          [ pure About <$> (Match.lit "about")
          , pure Home
          ]
      <* Match.end

-- | We have a `Maybe` here because the context has a default
-- | value and this would be a problem for the routing.
type RouterContext
  = ReactContext (Maybe RouterContextValue)

type RouterContextValue
  = { route :: Maybe AppRoute
    , nav :: PushStateInterface
    }
