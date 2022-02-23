module App.Pages.TodosListPage where

import Prelude
import App.Components.AddTodoInput (mkAddTodoInput)
import App.Components.Layout (mkLayout)
import App.Components.TodosList (mkTodosList)
import App.Components.TodosListNav (mkTodosListNav)
import App.Misc.Hook.UsePrev (usePrev)
import App.State.Helpers (useSelector)
import App.State.Selectors (todosMapSelector)
import App.State.Todo (TodoId, genUniqTodo)
import App.State.TodosMapReducer (addTodo, loadTodo)
import AppComponent (AppComponent, appComponent)
import Control.Monad.Reader (ask, lift)
import Data.Either (Either(..))
import Data.List (fromFoldable, length)
import Data.Map (lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import React.Basic.DOM as DOM
import React.Basic.Hooks (useContext, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)

type Props
  = { parentId :: TodoId }

mkTodosListPage :: AppComponent Props
mkTodosListPage = do
  { store, todosStorage } <- ask
  todosList <- mkTodosList
  addTodoInput <- liftEffect mkAddTodoInput
  layout <- lift mkLayout
  todosListNav <- mkTodosListNav
  appComponent "TodosListPage" \{ parentId } -> React.do
    todosMapState <- useSelector store.stateContext todosMapSelector
    dispatch <- useContext store.dispatchContext
    let
      maybeParent = lookup parentId todosMapState

      handleAdd :: String -> Effect Unit
      handleAdd text = do
        newTodo <- genUniqTodo parentId text false
        dispatch $ addTodo newTodo

      maybePrevious = case maybeParent of
        Just parent -> lookup parent.parent todosMapState
        _ -> Nothing

      showedTodos = fromMaybe (fromFoldable []) $ map _.children maybeParent

      computedTodosListNav = case (Tuple maybeParent maybePrevious) of
        (Tuple (Just parent) (Just previous)) ->
          [ todosListNav
              { parentTodo: parent
              , previousTodo: previous
              }
          ]
        _ -> []
    prevMaybeParent <- fromMaybe Nothing <$> usePrev maybeParent
    -- This is used to retrieve the parent
    -- from the storage.
    useAff parentId do
      eitherRetrievedParentTodo <- todosStorage.retrieve parentId
      case eitherRetrievedParentTodo of
        Right (Just retrievedParentTodo) -> liftEffect <<< dispatch $ loadTodo retrievedParentTodo
        _ -> pure unit
    -- This is used to synchronize the
    -- root todo with the storage.
    useAff (prevMaybeParent /\ maybeParent) do
      case (Tuple prevMaybeParent maybeParent) of
        (Tuple (Just prevParent) (Just parent)) ->
          if prevParent /= parent then
            todosStorage.store parent.id parent
          else
            pure (Right unit)
        _ -> pure (Right unit)
    pure
      $ layout
          [ DOM.div
              { className: "pt-40"
              , children:
                  [ DOM.div_ computedTodosListNav
                  , DOM.div
                      { className: "mt-4"
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
