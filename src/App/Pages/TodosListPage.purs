module App.Pages.TodosListPage where

import Prelude
import App.Components.AddTodoInput (mkAddTodoInput)
import App.Components.Breadcrumb (mkBreadcrumb)
import App.Components.Layout (mkLayout)
import App.Components.Navbar (mkNavbar)
import App.Components.TodosList (mkTodosList)
import App.Misc.Hook.UsePrev (usePrev)
import App.State.Helpers (useSelector)
import App.State.Selectors (todosMapSelector)
import App.State.Todo (TodoId, genUniqTodo, rootTodoId)
import App.State.TodosMapReducer (addTodo, loadTodo)
import AppComponent (AppComponent, appComponent)
import Control.Monad.Reader (ask, lift)
import Data.Either (Either(..))
import Data.List (List(..), length)
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
  navbar <- mkNavbar
  breadcrumb <- mkBreadcrumb
  appComponent "TodosListPage" \{ parentId } -> React.do
    todosMapState <- useSelector store.stateContext todosMapSelector
    dispatch <- useContext store.dispatchContext
    let
      maybeParent = lookup parentId todosMapState

      handleAdd :: String -> Effect Unit
      handleAdd text = do
        newTodo <- genUniqTodo parentId text false
        dispatch $ addTodo newTodo

      showedTodos = fromMaybe Nil $ map _.children maybeParent

      renderBreadcrumb = case (Tuple (parentId /= rootTodoId) maybeParent) of
        (Tuple true (Just currentTodo)) -> breadcrumb { currentTodo }
        _ -> DOM.div_ []
    prevMaybeParent <- fromMaybe Nothing <$> usePrev maybeParent
    -- This is used to retrieve the parent
    -- from the storage.
    useAff parentId do
      eitherRetrievedParentTodo <- todosStorage.retrieve parentId
      case eitherRetrievedParentTodo of
        Right (Just retrievedParentTodo) -> liftEffect <<< dispatch $ loadTodo retrievedParentTodo
        _ -> pure unit
    -- This is used to retrieve the previous
    -- todo, if it exists. The previous todo 
    -- is used in the breadcrump nav.
    useAff maybeParent do
      eitherRetrievedPreviousTodo <- case maybeParent of
        Just { parent } -> todosStorage.retrieve parent
        _ -> pure $ Left "Previous todo do not exists"
      case eitherRetrievedPreviousTodo of
        Right (Just retrievedPreviousTodo) -> liftEffect <<< dispatch $ loadTodo retrievedPreviousTodo
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
          ]
