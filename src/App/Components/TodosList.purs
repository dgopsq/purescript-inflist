module App.Components.TodosList where

import Prelude
import App.Components.ConnectedTodo (mkConnectedTodo)
import AppComponent (AppComponent, appComponent)
import Data.Array.NonEmpty (fromFoldable, toArray)
import Data.List (List)
import Data.Maybe (fromMaybe)
import React.Basic.DOM as DOM
import App.State.Todo (TodoId)

type Props
  = { todos :: List TodoId }

mkTodosList :: AppComponent Props
mkTodosList = do
  connectedTodo <- mkConnectedTodo
  appComponent "TodosList" \{ todos } -> React.do
    let
      maybeTodosArr =
        fromFoldable
          $ map
              ( \id ->
                  DOM.li
                    { className: "mb-4"
                    , children:
                        [ connectedTodo { id } ]
                    }
              )
              todos

      todosArr = fromMaybe [] $ map toArray maybeTodosArr
    pure
      $ DOM.ul_ todosArr
