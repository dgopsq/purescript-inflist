module Main
  ( main
  ) where

import Prelude
import App.Pages.Home (mkHome)
import AppEnv (AppComponent, appComponent)
import Control.Monad.Reader as Reader
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM (render)
import React.Basic.DOM as R
import React.Basic.Hooks as React
import React.Basic.Hooks as React.Basic
import Routes (AppRoute(..))
import Routes.Helpers (mkRouterContext, mkRouterProvider, useRouterContext)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  root <- getElementById "root" =<< (map toNonElementParentNode $ document =<< window)
  case root of
    Nothing -> throw "Root element not found."
    Just r -> do
      routerContext <- mkRouterContext
      let
        env = { routerContext }
      routerProvider <- Reader.runReaderT mkRouterProvider env
      app <- Reader.runReaderT mkApp env
      render (routerProvider [ app unit ]) r

-- | The main application component
-- | managing all the internal routes.
mkApp :: AppComponent Unit
mkApp = do
  { routerContext } <- Reader.ask
  home <- mkHome
  appComponent "App" \_ -> React.do
    { route } <- useRouterContext routerContext
    pure do
      React.Basic.fragment
        [ case route of
            Just Home -> home unit
            Just About -> R.h1_ [ R.text "About" ]
            Nothing -> R.div_ [ R.text "-" ]
        ]
