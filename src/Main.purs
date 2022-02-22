module Main where

import Prelude
import App.Api.Storage.LocalStorage (localTodosStorage)
import App.Pages.NotFoundPage (mkNotFoundPage)
import App.Pages.TodosListPage (mkTodosListPage)
import AppComponent (AppComponent, appComponent)
import Control.Monad.Reader (ask, runReaderT)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM (render)
import React.Basic.Hooks (fragment)
import React.Basic.Hooks as React
import Routes (AppRoute(..))
import Routes.Helpers (mkRouter, mkRouterProvider, useRouterContext)
import State.Helpers (mkStoreProvider)
import State.RootReducer (rootInitialState, rootReducer)
import State.Store (mkStore)
import State.Todo (rootTodoId)
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
        todosStorage = localTodosStorage

        env = { router, store, todosStorage }
      routerProvider <- runReaderT mkRouterProvider env
      storeProvider <- runReaderT (mkStoreProvider rootInitialState rootReducer) env
      app <- runReaderT mkApp env
      render (routerProvider [ storeProvider [ app unit ] ]) r

-- | The main application component
-- | managing all the internal routes.
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
