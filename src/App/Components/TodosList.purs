module App.Components.TodosList where

import Prelude
import App.Components.ConnectedTodo (mkConnectedTodo)
import AppComponent (AppComponent, appComponent)
import Data.Array.NonEmpty (fromFoldable, toArray)
import Data.List (List)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import React.Basic.DOM as DOM
import State.Todo (TodoId)

type Props
  = { todos :: List TodoId
    , onTodoChangeStatus :: TodoId -> Boolean -> Effect Unit
    }

mkTodosList :: AppComponent Props
mkTodosList = do
  connectedTodo <- mkConnectedTodo
  appComponent "TodosList" \{ todos, onTodoChangeStatus } -> React.do
    let
      maybeTodosArr =
        fromFoldable
          $ map
              ( \id ->
                  DOM.li_
                    [ connectedTodo
                        { id
                        , onChangeStatus: onTodoChangeStatus id
                        }
                    ]
              )
              todos

      todosArr = fromMaybe [] $ map toArray maybeTodosArr
    pure
      $ DOM.ul_ todosArr
