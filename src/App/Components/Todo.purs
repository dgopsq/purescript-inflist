module App.Components.Todo where

import Prelude
import App.Components.Checkbox (mkCheckbox)
import App.Foreign.TextareaAutosize (textareaAutosize)
import App.Foreign.UseClickOutside (useClickOutside)
import App.Misc.Hook.UseDebouncedEffect (useDebounce)
import App.State.Todo (Todo)
import Data.Maybe (fromMaybe)
import Data.String (null)
import Effect (Effect)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture, capture_, targetValue)
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (memo, useEffect, useMemo, useState, (/\))
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
      isFocused /\ setIsFocused <- useState false
      isWriting /\ setIsWriting <- useState false
      todoText /\ setTodoText <- useState todo.text
      todoNote /\ setTodoNote <- useState todo.note
      debouncedTodoText <- useDebounce 300 todoText
      debouncedTodoNote <- useDebounce 300 todoNote
      refClickOutside <- useClickOutside (setIsFocused \_ -> false)
      let
        handleChangeStatus :: Boolean -> Effect Unit
        handleChangeStatus updatedStatus = onChange (todo { checked = updatedStatus })

        handleTextChange :: EventHandler
        handleTextChange = capture targetValue \value -> setTodoText \_ -> fromMaybe "" value

        handleNoteChange :: EventHandler
        handleNoteChange = capture targetValue \value -> setTodoNote \_ -> fromMaybe "" value

        handleTodoClick :: EventHandler
        handleTodoClick = capture_ (setIsFocused \_ -> true)

        handleInputFocus :: EventHandler
        handleInputFocus = capture_ (setIsWriting \_ -> true)

        handleInputBlur :: EventHandler
        handleInputBlur = capture_ (setIsWriting \_ -> false)

        handleOpenTodo :: EventHandler
        handleOpenTodo = capture_ $ onOpen todo

        handleDeleteTodo :: EventHandler
        handleDeleteTodo = capture_ $ onDelete todo

        isEditing :: Boolean
        isEditing = isFocused || isWriting

        showBottomBar :: Boolean
        showBottomBar = isEditing

        showNoteInput :: Boolean
        showNoteInput = isEditing || (not <<< null) todoNote
      -- The `note` input will be displayed
      -- only when the Todo is in editing mode
      -- and when there actually a non-empty string.
      noteInput <-
        useMemo (showNoteInput /\ todoNote) \_ -> case showNoteInput of
          true ->
            DOM.div
              { className: "pl-8 pr-6 pt-2"
              , children:
                  [ textareaAutosize
                      { className: "w-full bg-transparent outline-black text-gray-600 text-sm resize-none"
                      , type: "text"
                      , placeholder: "Add notes..."
                      , value: todoNote
                      , onChange: handleNoteChange
                      , onFocus: handleInputFocus
                      , onBlur: handleInputBlur
                      }
                  ]
              }
          false -> DOM.div_ []
      -- In the bottom bar will be showed additional
      -- options, like the Todo deletion. This is visible
      -- only when the Todo is in editing mode.
      bottomBar <-
        useMemo showBottomBar \_ -> case showBottomBar of
          true ->
            DOM.div
              { className: "pl-8 pt-2"
              , children:
                  [ DOM.button
                      { className: "flex flex-row text-sm text-rose-500"
                      , onClick: handleDeleteTodo
                      , children:
                          [ DOM.i { className: "gg-trash gg-small mr-1", children: [] }
                          , DOM.text "Delete"
                          ]
                      }
                  ]
              }
          false -> DOM.div_ []
      useEffect (debouncedTodoText /\ debouncedTodoNote) do
        onChange
          ( todo
              { text = debouncedTodoText
              , note = debouncedTodoNote
              }
          )
        pure mempty
      pure
        $ DOM.div
            { ref: refClickOutside
            , onClick: handleTodoClick
            , className: "bg-gray-100 py-3 px-5 rounded-lg"
            , children:
                [ DOM.div
                    { className: "flex flex-row gap-x-3 items-center"
                    , children:
                        [ DOM.div
                            { className: "basis-auto grow-0 shrink-0 flex items-center"
                            , children:
                                [ checkbox
                                    { id: todo.id
                                    , checked: todo.checked
                                    , onChange: handleChangeStatus
                                    }
                                ]
                            }
                        , DOM.div
                            { className: "basis-full grow shrink"
                            , children:
                                [ DOM.input
                                    { className: "w-full bg-transparent outline-black"
                                    , type: "text"
                                    , value: todoText
                                    , onChange: handleTextChange
                                    , onFocus: handleInputFocus
                                    , onBlur: handleInputBlur
                                    }
                                ]
                            }
                        , DOM.div
                            { className: "basis-auto grow-0 shrink-0 flex items-center"
                            , children:
                                [ DOM.button
                                    { onClick: handleOpenTodo
                                    , children: [ DOM.i { className: "gg-external gg-normal text-black-500", children: [] } ]
                                    }
                                ]
                            }
                        ]
                    }
                , noteInput
                , bottomBar
                ]
            }
