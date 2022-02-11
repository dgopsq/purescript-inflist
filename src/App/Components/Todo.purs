module App.Components.Todo where

import Prelude
import AppEnv (AppComponent, appComponent)
import React.Basic.DOM as DOM
import State.TodosMapReducer (Todo)

mkTodo :: AppComponent { todo :: Todo }
mkTodo =
  appComponent "Todo" \{ todo } -> React.do
    pure
      $ DOM.div_
          [ DOM.div_ [ DOM.input { type: "checkbox", checked: todo.checked } ]
          , DOM.div_ [ DOM.text todo.text ]
          ]
