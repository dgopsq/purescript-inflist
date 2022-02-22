module App.Components.ConnectedTodo where

import Prelude
import App.Components.Todo (mkTodo)
import AppComponent (AppComponent, appComponent)
import Control.Monad.Reader (ask)
import Data.Map (lookup)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import React.Basic.DOM as DOM
import React.Basic.Hooks (useContext, useMemo, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import App.Routes.Helpers (navigateTo, useRouterContext)
import App.State.Helpers (useSelector)
import App.State.Selectors (todosMapSelector)
import App.State.Todo (TodoId, Todo)
import App.State.TodosMapReducer (loadTodo, updateTodo)

type Props
  = { id :: TodoId }

mkConnectedTodo :: AppComponent Props
mkConnectedTodo = do
  { store, todosStorage, router } <- ask
  todo <- liftEffect mkTodo
  appComponent "ConnectedTodo" \{ id } -> React.do
    todosMapState <- useSelector store.stateContext todosMapSelector
    dispatch <- useContext store.dispatchContext
    { nav } <- useRouterContext router.routerContext
    let
      maybeTodo = lookup id todosMapState

      handleUpdate :: Todo -> Effect Unit
      handleUpdate updatedTodo = dispatch $ updateTodo updatedTodo.id updatedTodo

      handleOpen :: Todo -> Effect Unit
      handleOpen todoToOpen = navigateTo nav $ "/" <> todoToOpen.id

      handleDelete :: Todo -> Effect Unit
      handleDelete _ = pure unit
    memoizedHandleUpdate <- useMemo unit \_ -> handleUpdate
    memoizedHandleOpen <- useMemo unit \_ -> handleOpen
    memoizedHandleDelete <- useMemo unit \_ -> handleDelete
    -- Retrieve from the storage the missing todo
    retrievedTodo <-
      map isJust
        $ useAff id do
            maybeRetrievedTodo <- todosStorage.retrieve id
            case maybeRetrievedTodo of
              Just retrievedTodo -> liftEffect <<< dispatch $ loadTodo retrievedTodo
              _ -> pure unit
    -- Update the todo into the storage
    -- This hook will be triggered at the first
    -- render too, causing and additional and useless
    -- storage request.
    useAff (retrievedTodo /\ maybeTodo) do
      case (Tuple retrievedTodo maybeTodo) of
        (Tuple true (Just updatedTodo)) -> todosStorage.store updatedTodo.id updatedTodo
        _ -> pure unit
    pure $ fromMaybe (DOM.div_ [])
      $ map
          ( \t ->
              React.element todo
                { todo: t
                , onChange: memoizedHandleUpdate
                , onOpen: memoizedHandleOpen
                , onDelete: memoizedHandleDelete
                }
          )
          maybeTodo
