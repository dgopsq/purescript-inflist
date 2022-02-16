module App.Components.ConnectedTodo where

import Prelude
import App.Components.Todo (mkTodo)
import AppComponent (AppComponent, appComponent)
import Control.Monad.Reader (ask)
import Data.Map (lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import React.Basic.DOM as DOM
import React.Basic.Hooks (useContext)
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import State.Helpers (useSelector)
import State.Selectors (todosMapSelector)
import State.Todo (TodoId)
import State.TodosMapReducer (loadTodo)

type Props
  = { id :: TodoId
    , onChangeStatus :: Boolean -> Effect Unit
    }

mkConnectedTodo :: AppComponent Props
mkConnectedTodo = do
  { store, todosStorage } <- ask
  todo <- mkTodo
  appComponent "ConnectedTodo" \{ id, onChangeStatus } -> React.do
    todosMapState <- useSelector store.stateContext todosMapSelector
    dispatch <- useContext store.dispatchContext
    let
      maybeTodo = lookup id todosMapState
    -- Retrieve from the storage the missing todo
    useAff [ id ] do
      maybeRetrievedTodo <- todosStorage.retrieve id
      case maybeRetrievedTodo of
        Just retrievedTodo -> liftEffect <<< dispatch $ loadTodo retrievedTodo
        _ -> pure unit
    -- Update the todo into the storage
    useAff [ maybeTodo ] do
      case maybeTodo of
        Just updatedTodo -> todosStorage.store updatedTodo.id updatedTodo
        _ -> pure unit
    pure $ fromMaybe (DOM.div_ []) $ map (\t -> todo { todo: t, onChangeStatus }) maybeTodo
