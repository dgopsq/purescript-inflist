module App.Components.ConnectedTodo where

import Prelude
import App.Components.Todo (mkTodo)
import AppComponent (AppComponent, appComponent)
import Control.Monad.Reader (ask)
import Data.Map (lookup)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import React.Basic.DOM as DOM
import React.Basic.Hooks as React
import State.Helpers (useSelector)
import State.Selectors (todosMapSelector)
import State.Todo (TodoId)

type Props
  = { id :: TodoId, onChangeStatus :: Boolean -> Effect Unit }

mkConnectedTodo :: AppComponent Props
mkConnectedTodo = do
  { store } <- ask
  todo <- mkTodo
  appComponent "ConnectedTodo" \{ id, onChangeStatus } -> React.do
    todosMapState <- useSelector store.stateContext todosMapSelector
    let
      maybeTodo = lookup id todosMapState
    pure $ fromMaybe (DOM.div_ []) $ map (\t -> todo { todo: t, onChangeStatus }) maybeTodo
