module App.Components.Link where

import Prelude
import AppEnv (AppComponent, appComponent)
import Control.Monad.Reader (ask)
import Foreign (unsafeToForeign)
import React.Basic.DOM as DOM
import React.Basic.Hooks as React
import React.Basic.DOM.Events (capture_)
import Routes.Helpers (useRouterContext)

mkLink :: AppComponent { route :: String, text :: String }
mkLink = do
  { routerContext } <- ask
  appComponent "Link" \{ route, text } -> React.do
    { nav } <- useRouterContext routerContext
    let
      handlePress = capture_ $ nav.pushState (unsafeToForeign {}) route
    pure $ DOM.a { href: "#", onClick: handlePress, children: [ DOM.text text ] }
