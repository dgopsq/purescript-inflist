module App.Components.AddTodoInput where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (key, targetValue)
import React.Basic.Events (SyntheticEvent, handler, merge)
import React.Basic.Hooks (memo, useState, (/\))
import React.Basic.Hooks as React

type Props
  = { onAdd :: String -> Effect Unit }

-- | A purely visual component rendering the 
-- | text input used to create a new Todo.
mkAddTodoInput :: Effect (React.ReactComponent Props)
mkAddTodoInput =
  memo do
    React.reactComponent "AddTodoInput" \{ onAdd } -> React.do
      value /\ setValue <- useState ""
      let
        handleAdd :: EffectFn1 SyntheticEvent Unit
        handleAdd =
          handler (merge { key, targetValue }) \event -> case event of
            { key: Just "Enter", targetValue: Just v } -> do
              _ <- onAdd v
              _ <- setValue \_ -> ""
              pure unit
            _ -> pure unit

        handleChange :: EffectFn1 SyntheticEvent Unit
        handleChange =
          handler targetValue \maybeValue -> case maybeValue of
            Just v -> setValue \_ -> v
            _ -> pure unit
      pure
        $ DOM.input
            { type: "text"
            , className: "bg-gray-100 py-3 px-5 rounded-lg w-full outline-black placeholder:text-gray-300"
            , placeholder: "New todo..."
            , autoFocus: true
            , value
            , onChange: handleChange
            , onKeyUp: handleAdd
            }
