module App.Components.AddTodoInput where

import Prelude
import AppEnv (AppComponent, appComponent)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (key, targetValue)
import React.Basic.Events (SyntheticEvent, handler, merge)

type Props
  = { onAdd :: String -> Effect Unit }

mkAddTodoInput :: AppComponent Props
mkAddTodoInput =
  appComponent "AddTodoInput" \{ onAdd } -> React.do
    let
      handleAdd :: EffectFn1 SyntheticEvent Unit
      handleAdd =
        handler (merge { key, targetValue }) \event -> case event of
          { key: Just "Enter", targetValue: Just v } -> onAdd v
          _ -> pure unit
    pure
      $ DOM.input { type: "text", autoFocus: true, onKeyUp: handleAdd }
