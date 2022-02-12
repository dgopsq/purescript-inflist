module App.Components.Todo where

import Prelude
import AppEnv (AppComponent, appComponent)
import React.Basic.DOM as DOM
import State.TodosMapReducer (Todo)

mkTodo :: AppComponent { todo :: Todo }
mkTodo =
  appComponent "Todo" \{ todo } -> React.do
    pure
      $ DOM.div
          { className: "flex flex-row gap-x-3 align-center"
          , children:
              [ DOM.div_ [ DOM.input { type: "checkbox", checked: todo.checked } ]
              , DOM.div_ [ DOM.text todo.text ]
              ]
          }
