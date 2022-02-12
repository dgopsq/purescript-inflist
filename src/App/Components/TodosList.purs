module App.Components.TodosList where

import Prelude
import App.Components.Todo (mkTodo)
import AppEnv (AppComponent, appComponent)
import Data.Array.NonEmpty (fromFoldable, toArray)
import Data.List (List)
import Data.Maybe (fromMaybe)
import React.Basic.DOM as DOM
import State.TodosMapReducer (Todo)

mkTodosList :: AppComponent { todos :: List Todo }
mkTodosList = do
  todo <- mkTodo
  appComponent "TodosList" \{ todos } -> React.do
    let
      maybeTodosArr = fromFoldable $ map (\t -> todo { todo: t }) todos

      todosArr = fromMaybe [] $ map toArray maybeTodosArr
    pure
      $ DOM.div_ todosArr
