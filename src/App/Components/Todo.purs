module App.Components.Todo where

import Prelude
import AppEnv (AppComponent, appComponent)
import Effect.Uncurried (EffectFn1)
import React.Basic.DOM as DOM
import React.Basic.Events (SyntheticEvent)
import State.TodosMapReducer (Todo)

type Props
  = { todo :: Todo, onChangeStatus :: EffectFn1 SyntheticEvent Unit }

mkTodo :: AppComponent Props
mkTodo =
  appComponent "Todo" \{ todo, onChangeStatus } -> React.do
    pure
      $ DOM.div
          { className: "flex flex-row gap-x-3 align-center"
          , children:
              [ DOM.div_
                  [ DOM.input
                      { type: "checkbox"
                      , checked: todo.checked
                      , onClick: onChangeStatus
                      }
                  ]
              , DOM.div_ [ DOM.text todo.text ]
              ]
          }
