module App.Components.ConnectedTodo where

import Prelude
import App.Components.Todo (mkTodo)
import App.Routes.Helpers (navigateTo, useRouterContext)
import App.State.Helpers (useSelector)
import App.State.Selectors (todosMapSelector)
import App.State.Todo (TodoId, Todo)
import App.State.TodosMapReducer (deleteTodo, loadTodo, updateTodo)
import AppComponent (AppComponent, appComponent)
import Control.Monad.Reader (ask)
import Data.Either (Either(..))
import Data.Map (lookup)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import React.Basic.DOM as DOM
import React.Basic.Hooks (useContext, useMemo, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)

type Props
  = { id :: TodoId }

-- | A `ConnectedTodo` is a wrapper for te `Todo`
-- | component that connects all the states and events
-- | of a single Todo with the in-memory state and storage.
-- |
-- | This component will load the described Todo from the
-- | storage if it's not present in the current state and
-- | will handle the update behaviour after editing the
-- | wrapped Todo.
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
      handleDelete todoToDelete = do
        _ <- dispatch $ deleteTodo todoToDelete
        _ <- launchAff_ $ todosStorage.delete todoToDelete.id
        pure unit
    memoizedHandleUpdate <- useMemo unit \_ -> handleUpdate
    memoizedHandleOpen <- useMemo unit \_ -> handleOpen
    memoizedHandleDelete <- useMemo unit \_ -> handleDelete
    -- This  hook is used to retrieve the current Todo 
    -- from the storage.
    -- At the component's first render this hook
    -- will be triggered synchronizing the in-memory
    -- Todo with the stored one.
    retrievedTodo <-
      map isJust
        $ useAff id do
            eitherRetrievedTodo <- todosStorage.retrieve id
            case eitherRetrievedTodo of
              Right (Just retrievedTodo) -> liftEffect <<< dispatch $ loadTodo retrievedTodo
              _ -> pure unit
    -- Here the described Todo will be updated inside the storage
    -- if, and only if there was a change.
    -- Passing the `retrievedTodo` value as a dependency
    -- and checking its truthness will assure that the hook
    -- won't be triggered on the very first render.
    useAff (retrievedTodo /\ maybeTodo) do
      case (Tuple retrievedTodo maybeTodo) of
        (Tuple true (Just updatedTodo)) -> todosStorage.store updatedTodo.id updatedTodo
        _ -> pure (Right unit)
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
