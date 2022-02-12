module App.Components.TodosList where

import Prelude
import App.Components.Todo (mkTodo)
import AppEnv (AppComponent, appComponent)
import Data.Array.NonEmpty (fromFoldable, toArray)
import Data.List (List)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import React.Basic.DOM as DOM
import React.Basic.Events (SyntheticEvent)
import State.TodosMapReducer (Todo, TodoId)

type Props
  = { todos :: List Todo
    , onTodoChangeStatus :: TodoId -> Boolean -> Effect Unit
    }

mkTodosList :: AppComponent Props
mkTodosList = do
  todo <- mkTodo
  appComponent "TodosList" \{ todos, onTodoChangeStatus } -> React.do
    let
      handleTodoChangeStatus :: Todo -> EffectFn1 SyntheticEvent Unit
      handleTodoChangeStatus t = mkEffectFn1 $ \_ -> onTodoChangeStatus t.id (not t.checked)

      maybeTodosArr =
        fromFoldable
          $ map
              ( \t ->
                  DOM.li_
                    [ todo
                        { todo: t
                        , onChangeStatus: handleTodoChangeStatus t
                        }
                    ]
              )
              todos

      todosArr = fromMaybe [] $ map toArray maybeTodosArr
    pure
      $ DOM.ul_ todosArr
