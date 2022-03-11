module App.Components.SingleTodoHeader
  ( mkSingleTodoHeader
  ) where

import Prelude
import App.Foreign.TextareaAutosize (textareaAutosize)
import App.Misc.Hook.UseDebouncedEffect (useDebounce)
import App.Misc.Hook.UsePrev (usePrev)
import App.State.Todo (Todo)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture, targetValue)
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (Component, component, useEffect, useState, (/\))
import React.Basic.Hooks as React

type Props
  = { todo :: Todo
    , onChange :: Todo -> Effect Unit
    }

-- | The header component for a single Todo, containing
-- | two inputs one for the Todo text and the other for
-- | the notes. This is mainly showed in the `TodoListPage`
-- | when the parent Todo is not the root.
mkSingleTodoHeader :: Component Props
mkSingleTodoHeader = do
  component "SingleTodoHeader" \{ todo, onChange } -> React.do
    todoText /\ setTodoText <- useState todo.text
    todoNote /\ setTodoNote <- useState todo.note
    debouncedTodoText <- useDebounce 300 todoText
    debouncedTodoNote <- useDebounce 300 todoNote
    maybePrevTodoId <- usePrev todo.id
    let
      handleTextChange :: EventHandler
      handleTextChange = capture targetValue \value -> setTodoText \_ -> fromMaybe "" value

      handleNoteChange :: EventHandler
      handleNoteChange = capture targetValue \value -> setTodoNote \_ -> fromMaybe "" value
    useEffect (debouncedTodoText /\ debouncedTodoNote) do
      onChange
        ( todo
            { text = debouncedTodoText
            , note = debouncedTodoNote
            }
        )
      pure mempty
    useEffect (maybePrevTodoId /\ todo.id)
      $ if maybePrevTodoId /= (Just todo.id) then do
          _ <- setTodoText \_ -> todo.text
          _ <- setTodoNote \_ -> todo.note
          pure mempty
        else
          pure mempty
    pure
      $ DOM.div_
          [ DOM.input
              { className: "text-xl font-bold w-full outline-black"
              , value: todoText
              , onChange: handleTextChange
              }
          , DOM.div
              { className: "mt-2"
              , children:
                  [ textareaAutosize
                      { className: "w-full bg-transparent outline-black resize-none"
                      , value: todoNote
                      , onChange: handleNoteChange
                      , placeholder: "Add notes..."
                      }
                  ]
              }
          ]
