module App.Components.Todo where

import Prelude
import App.Components.Checkbox (mkCheckbox)
import App.Components.Link (mkLink)
import AppComponent (AppComponent, appComponent)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Misc.Hook.UseDebouncedEffect (useDebounce)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (useEffect, useState, (/\))
import React.Basic.Hooks as React
import State.Todo (Todo)

type Props
  = { todo :: Todo
    , onChange :: Todo -> Effect Unit
    }

mkTodo :: AppComponent Props
mkTodo = do
  checkbox <- mkCheckbox
  link <- mkLink
  appComponent "Todo" \{ todo, onChange } -> React.do
    todoText /\ setTodoText <- useState todo.text
    debouncedTodoText <- useDebounce 300 todoText
    let
      handleChangeStatus :: Boolean -> Effect Unit
      handleChangeStatus updatedStatus = onChange (todo { checked = updatedStatus })
    useEffect debouncedTodoText do
      onChange (todo { text = todoText })
      pure mempty
    pure
      $ DOM.div
          { className: "bg-white py-2 px-4 rounded flex flex-row gap-x-3 items-center"
          , children:
              [ DOM.div
                  { className: "basis-auto grow-0 shrink-0 flex items-center"
                  , children:
                      [ checkbox
                          { checked: todo.checked
                          , onChange: handleChangeStatus
                          }
                      ]
                  }
              , DOM.div
                  { className: "basis-full grow shrink"
                  , children:
                      [ DOM.input
                          { className: "w-full"
                          , type: "text"
                          , value: todoText
                          , onChange:
                              handler targetValue \value -> setTodoText \_ -> fromMaybe "" value
                          }
                      ]
                  }
              , DOM.div
                  { className: "basis-auto grow-0 shrink-0"
                  , children:
                      [ link
                          { route: "/" <> todo.id
                          , children:
                              [ DOM.i { className: "gg-external gg-normal text-slate-300", children: [] }
                              ]
                          }
                      ]
                  }
              ]
          }
