module App.Components.Todo where

import Prelude
import App.Components.Link (mkLink)
import AppComponent (AppComponent, appComponent)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import React.Basic.DOM as DOM
import State.Todo (Todo)

type Props
  = { todo :: Todo, onChangeStatus :: Boolean -> Effect Unit }

mkTodo :: AppComponent Props
mkTodo = do
  link <- mkLink
  appComponent "Todo" \{ todo, onChangeStatus } -> React.do
    pure
      $ DOM.div
          { className: "flex flex-row gap-x-3 align-center"
          , children:
              [ DOM.div_
                  [ DOM.input
                      { type: "checkbox"
                      , checked: todo.checked
                      , onChange: mkEffectFn1 \_ -> onChangeStatus $ not todo.checked
                      }
                  ]
              , DOM.div_
                  [ link
                      { route: "/" <> todo.id
                      , text: todo.text
                      }
                  ]
              ]
          }
