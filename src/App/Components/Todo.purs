module App.Components.Todo where

import Prelude
import App.Components.Checkbox (mkCheckbox)
import App.Components.Link (mkLink)
import AppComponent (AppComponent, appComponent)
import Effect (Effect)
import React.Basic.DOM as DOM
import State.Todo (Todo)

type Props
  = { todo :: Todo, onChangeStatus :: Boolean -> Effect Unit }

mkTodo :: AppComponent Props
mkTodo = do
  link <- mkLink
  checkbox <- mkCheckbox
  appComponent "Todo" \{ todo, onChangeStatus } -> React.do
    pure
      $ DOM.div
          { className: "bg-white py-2 px-4 rounded flex flex-row gap-x-3 items-center"
          , children:
              [ checkbox
                  { checked: todo.checked
                  , onChange: onChangeStatus
                  }
              , DOM.div_
                  [ link
                      { route: "/" <> todo.id
                      , text: todo.text
                      }
                  ]
              ]
          }
