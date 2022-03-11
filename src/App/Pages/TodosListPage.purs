module App.Pages.TodosListPage where

import Prelude
import App.Components.AddTodoInput (mkAddTodoInput)
import App.Components.Breadcrumb (mkBreadcrumb)
import App.Components.Layout (mkLayout)
import App.Components.Navbar (mkNavbar)
import App.Components.SingleTodoHeader (mkSingleTodoHeader)
import App.Components.TodosList (mkTodosList)
import App.Misc.Hook.UsePrev (usePrev)
import App.State.Helpers (useSelector)
import App.State.Selectors (todosMapSelector)
import App.State.Todo (Todo, TodoId, genUniqTodo, rootTodoId)
import App.State.TodosMapReducer (addTodo, loadTodo, updateTodo)
import AppComponent (AppComponent, appComponent)
import Control.Monad.Reader (ask, lift)
import Data.Either (Either(..))
import Data.List (List(..), length)
import Data.Map (lookup)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import React.Basic.DOM as DOM
import React.Basic.Hooks (useContext, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)

type Props
  = { parentId :: TodoId }

-- | The component that will render a list of Todos
-- | based on the parent Todo given.
-- | This is the "main" application's page which is
-- | used for both the "homepage" and the single Todo page.
mkTodosListPage :: AppComponent Props
mkTodosListPage = do
  { store, todosStorage } <- ask
  todosList <- mkTodosList
  addTodoInput <- liftEffect mkAddTodoInput
  layout <- lift mkLayout
  navbar <- mkNavbar
  breadcrumb <- mkBreadcrumb
  singleTodoHeader <- lift mkSingleTodoHeader
  appComponent "TodosListPage" \{ parentId } -> React.do
    todosMapState <- useSelector store.stateContext todosMapSelector
    dispatch <- useContext store.dispatchContext
    let
      maybeParent = lookup parentId todosMapState

      handleAdd :: String -> Effect Unit
      handleAdd text = do
        newTodo <- genUniqTodo parentId text false
        dispatch $ addTodo newTodo

      handleUpdate :: Todo -> Effect Unit
      handleUpdate updatedTodo = dispatch $ updateTodo updatedTodo.id updatedTodo

      showedTodos = fromMaybe Nil $ map _.children maybeParent

      renderBreadcrumb = case (Tuple (parentId /= rootTodoId) maybeParent) of
        (Tuple true (Just currentTodo)) -> breadcrumb { currentTodo }
        _ -> DOM.div_ []

      renderHeader = case maybeParent of
        Just parent ->
          if parent.id /= rootTodoId then
            singleTodoHeader { todo: parent, onChange: handleUpdate }
          else
            DOM.div_ []
        Nothing -> DOM.div_ []
    prevMaybeParent <- fromMaybe Nothing <$> usePrev maybeParent
    -- This  hook is used to retrieve the parent
    -- Todo from the storage.
    -- At the application's first render this hook
    -- will be triggered synchronizing the in-memory
    -- parent Todo with the stored one.
    useAff parentId do
      eitherRetrievedParentTodo <- todosStorage.retrieve parentId
      case eitherRetrievedParentTodo of
        Right (Just retrievedParentTodo) -> liftEffect <<< dispatch $ loadTodo retrievedParentTodo
        _ -> pure unit
    -- This hook is used to retrieve the previous
    -- todo, if it exists.
    -- The previous Todo is the "parent's parent" and
    -- it is used in the breadcrumb menu to track the
    -- position inside the nested Todos.
    useAff maybeParent do
      eitherRetrievedPreviousTodo <- case maybeParent of
        Just { parent } -> do
          isPreviousLoaded <- pure <<< isJust $ lookup parent todosMapState
          case isPreviousLoaded of
            false -> todosStorage.retrieve parent
            true -> pure $ Left "Previous todo already loaded"
        _ -> pure $ Left "Parent todo do not exists"
      case eitherRetrievedPreviousTodo of
        Right (Just retrievedPreviousTodo) -> liftEffect <<< dispatch $ loadTodo retrievedPreviousTodo
        _ -> pure unit
    -- Here the root todo will be updated inside the storage
    -- if, and only if there was a change.
    -- Using the previous version of the parent in the
    -- dependencies, and comparing it later will assure that
    -- the hook won't be triggered on the very first render.
    useAff (prevMaybeParent /\ maybeParent) do
      case (Tuple prevMaybeParent maybeParent) of
        (Tuple (Just prevParent) (Just parent)) ->
          if prevParent /= parent then
            todosStorage.store parent.id parent
          else
            pure (Right unit)
        _ -> pure (Right unit)
    pure
      $ DOM.div_
          [ navbar { parentId }
          , layout
              [ DOM.div
                  { className: "pt-10"
                  , children:
                      [ DOM.div
                          { className: ""
                          , children:
                              [ renderBreadcrumb ]
                          }
                      , DOM.div
                          { className: "mt-6"
                          , children: [ renderHeader ]
                          }
                      , DOM.div
                          { className: "mt-8"
                          , children:
                              [ React.element addTodoInput { onAdd: handleAdd }
                              ]
                          }
                      , DOM.div
                          { className: if length showedTodos > 0 then "mt-4" else ""
                          , children:
                              [ todosList { todos: showedTodos }
                              ]
                          }
                      ]
                  }
              ]
          ]
