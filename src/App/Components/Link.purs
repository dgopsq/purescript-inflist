module App.Components.Link where

import Prelude
import App.Foreign.EnvConfig (getRootDir)
import App.Routes.Helpers (navigateTo, useRouterContext)
import AppComponent (AppComponent, appComponent)
import Control.Monad.Reader (ask)
import Data.Maybe (fromMaybe)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (JSX)
import React.Basic.Hooks as React

type Props
  = { route :: String
    , children :: Array JSX
    }

-- | A `Link` component is just an anchor that
-- | allows to navigate without refreshing the page
-- | using the navigation interface from the application's
-- | dependencies.
mkLink :: AppComponent Props
mkLink = do
  { router } <- ask
  appComponent "Link" \{ route, children } -> React.do
    { nav } <- useRouterContext router.routerContext
    let
      computedRoute = fromMaybe "" getRootDir <> route

      handlePress = capture_ $ navigateTo nav computedRoute
    pure $ DOM.a { href: "#", onClick: handlePress, children }
