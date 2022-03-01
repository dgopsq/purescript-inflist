module App.Components.Todo where

import Prelude
import App.Components.Checkbox (mkCheckbox)
import App.Foreign.UseClickOutside (useClickOutside)
import App.Misc.Hook.UseDebouncedEffect (useDebounce)
import App.State.Todo (Todo)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (JSX, memo, useEffect, useEffectAlways, useState, (/\))
import React.Basic.Hooks as React

type Props
  = { todo :: Todo
    , onChange :: Todo -> Effect Unit
    , onOpen :: Todo -> Effect Unit
    , onDelete :: Todo -> Effect Unit
    }

-- | A purely visual component rendering a Todo.
-- | This will handle all the basic HTML events
-- | that will be triggered while dealing with a single
-- | Todo. This component should be used through the 
-- | `ConnectedTodo` to have it synchronized with the
-- | application's state and storage.
mkTodo :: Effect (React.ReactComponent Props)
mkTodo =
  memo do
    checkbox <- mkCheckbox
    React.reactComponent "Todo" \{ todo, onChange, onOpen, onDelete } -> React.do
      isEditing /\ setIsEditing <- useState false
      todoText /\ setTodoText <- useState todo.text
      debouncedTodoText <- useDebounce 300 todoText
      refClickOutside <- useClickOutside (setIsEditing \_ -> false)
      let
        handleChangeStatus :: Boolean -> Effect Unit
        handleChangeStatus updatedStatus = onChange (todo { checked = updatedStatus })

        rightIcon :: JSX
        rightIcon = case isEditing of
          true ->
            DOM.a
              { href: "#"
              , onClick: handler_ $ onDelete todo
              , children: [ DOM.i { className: "gg-trash gg-normal text-rose-500", children: [] } ]
              }
          false ->
            DOM.a
              { href: "#"
              , onClick: handler_ $ onOpen todo
              , children: [ DOM.i { className: "gg-external gg-normal text-indigo-500", children: [] } ]
              }
      useEffect debouncedTodoText do
        onChange (todo { text = todoText })
        pure mempty
      useEffectAlways do
        _ <- log $ "Rerended todo" <> (show todo.id)
        pure mempty
      pure
        $ DOM.div
            { className: "bg-white border border-indigo-100 py-3 px-5 rounded flex flex-row gap-x-3 items-center"
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
                    { ref: refClickOutside
                    , className: "basis-full grow shrink flex flex-row gap-x-3 items-center"
                    , children:
                        [ DOM.div
                            { className: "basis-full grow shrink"
                            , children:
                                [ DOM.input
                                    { className: "w-full outline-indigo-300"
                                    , type: "text"
                                    , value: todoText
                                    , onChange:
                                        handler targetValue \value -> setTodoText \_ -> fromMaybe "" value
                                    , onFocus: handler_ (setIsEditing \_ -> true)
                                    }
                                ]
                            }
                        , DOM.div
                            { className: "basis-auto grow-0 shrink-0"
                            , children: [ rightIcon ]
                            }
                        ]
                    }
                ]
            }
