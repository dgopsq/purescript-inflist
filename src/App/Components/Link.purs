module App.Components.Link where

import Prelude
import AppComponent (AppComponent, appComponent)
import Control.Monad.Reader (ask)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (JSX)
import React.Basic.Hooks as React
import App.Routes.Helpers (navigateTo, useRouterContext)

type Props
  = { route :: String
    , children :: Array JSX
    }

mkLink :: AppComponent Props
mkLink = do
  { router } <- ask
  appComponent "Link" \{ route, children } -> React.do
    { nav } <- useRouterContext router.routerContext
    let
      handlePress = capture_ $ navigateTo nav route
    pure $ DOM.a { href: "#", onClick: handlePress, children }
