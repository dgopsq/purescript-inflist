module Main where

import Prelude
import App.Pages.About (mkAbout)
import App.Pages.Home (mkHome)
import App.Pages.NotFound (mkNotFound)
import AppEnv (AppComponent, appComponent)
import Control.Monad.Reader (ask, runReaderT)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM (render)
import React.Basic.DOM as R
import React.Basic.Hooks (fragment)
import React.Basic.Hooks as React
import Routes (AppRoute(..))
import Routes.Helpers (mkRouter, mkRouterProvider, useRouterContext)
import State.Helpers (mkStoreProvider)
import State.RootReducer (rootInitialState, rootReducer)
import State.Store (mkStore)
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
      router <- mkRouter
      store <- mkStore
      let
        env = { router, store }
      routerProvider <- runReaderT mkRouterProvider env
      storeProvider <- runReaderT (mkStoreProvider rootInitialState rootReducer) env
      app <- runReaderT mkApp env
      render (routerProvider [ storeProvider [ app unit ] ]) r

-- | The main application component
-- | managing all the internal routes.
mkApp :: AppComponent Unit
mkApp = do
  { router } <- ask
  home <- mkHome
  about <- mkAbout
  notFound <- mkNotFound
  appComponent "App" \_ -> React.do
    { route } <- useRouterContext router.routerContext
    pure do
      fragment
        [ case route of
            Just Home -> home unit
            Just About -> about unit
            Nothing -> notFound unit
        ]
