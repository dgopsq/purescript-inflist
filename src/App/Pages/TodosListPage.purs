module App.Pages.TodosListPage where

import Prelude
import App.Components.TodosList (mkTodosList)
import AppEnv (AppComponent, appComponent)
import Control.Monad.Reader (ask)
import Data.Map (values)
import React.Basic.DOM as DOM
import React.Basic.Hooks as React
import State.Helpers (useSelector)
import State.Selectors (todosMapSelector)

mkTodosListPage :: AppComponent Unit
mkTodosListPage = do
  { store } <- ask
  todosList <- mkTodosList
  appComponent "TodosListPage" \_ -> React.do
    todosMapState <- useSelector store.stateContext todosMapSelector
    pure
      $ DOM.div_
          [ DOM.h1_ [ DOM.text "Todos List" ]
          , todosList { todos: values todosMapState }
          ]
