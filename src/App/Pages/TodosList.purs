module App.Pages.TodosList where

import Prelude
import App.Components.Todo (mkTodo)
import AppEnv (AppComponent, appComponent)
import Control.Monad.Reader (ask)
import Data.Array.NonEmpty (fromFoldable, toArray)
import Data.Map (values)
import Data.Maybe (fromMaybe)
import React.Basic.DOM as DOM
import React.Basic.Hooks as React
import State.Helpers (useSelector)
import State.Selectors (todosMapSelector)

mkTodosList :: AppComponent Unit
mkTodosList = do
  { store } <- ask
  todo <- mkTodo
  appComponent "TodosList" \_ -> React.do
    todosMapState <- useSelector store.stateContext todosMapSelector
    let
      maybeTodos = fromFoldable $ map (\t -> todo { todo: t }) (values todosMapState)

      todos = fromMaybe [] $ map toArray maybeTodos
    pure
      $ DOM.div_
          [ DOM.h1_ [ DOM.text "Todos List" ]
          , DOM.div_ todos
          ]
