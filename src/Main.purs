module Main where

import Prelude
import App.Api.Storage.LocalStorage (localTodosStorage)
import App.Pages.NotFoundPage (mkNotFoundPage)
import App.Pages.TodosListPage (mkTodosListPage)
import App.Routes (AppRoute(..))
import App.Routes.Helpers (mkRouter, mkRouterProvider, useRouterContext)
import App.State.Helpers (mkStoreProvider)
import App.State.RootReducer (rootInitialState, rootReducer)
import App.State.Store (mkStore)
import App.State.Todo (rootTodoId)
import AppComponent (AppComponent, appComponent)
import Control.Monad.Reader (ask, runReaderT)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM (render)
import React.Basic.Hooks (fragment)
import React.Basic.Hooks as React
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

-- | This is the application's entry point. All the
-- | dependencies are going to be defined and injected
-- | to be used deep into the application's logic.
-- | In this function the actual wiring between React and
-- | the HTML DOM will be executed using the element with the
-- | id equal to "root".
main :: Effect Unit
main = do
  root <- getElementById "root" =<< (map toNonElementParentNode $ document =<< window)
  case root of
    Nothing -> throw "Root element not found."
    Just r -> do
      router <- mkRouter
      store <- mkStore
      let
        todosStorage = localTodosStorage

        env = { router, store, todosStorage }
      routerProvider <- runReaderT (mkRouterProvider Nothing) env
      storeProvider <- runReaderT (mkStoreProvider rootInitialState rootReducer) env
      app <- runReaderT mkApp env
      render (routerProvider [ storeProvider [ app unit ] ]) r

-- | This is the application's main component.
-- | Here all the routes will be wired with the relative
-- | components.
mkApp :: AppComponent Unit
mkApp = do
  { router } <- ask
  todosListPage <- mkTodosListPage
  notFoundPage <- mkNotFoundPage
  appComponent "App" \_ -> React.do
    { route } <- useRouterContext router.routerContext
    pure do
      fragment
        [ case route of
            Just RootTodos -> todosListPage { parentId: rootTodoId }
            Just (ChildrenTodos todoId) -> todosListPage { parentId: todoId }
            Nothing -> notFoundPage unit
        ]
