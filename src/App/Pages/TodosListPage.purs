module App.Pages.TodosListPage where

import Prelude
import App.Components.AddTodoInput (mkAddTodoInput)
import App.Components.Layout (mkLayout)
import App.Components.TodosList (mkTodosList)
import App.Components.TodosListNav (mkTodosListNav)
import AppEnv (AppComponent, appComponent)
import Control.Monad.Reader (ask)
import Data.List (fromFoldable, length, mapMaybe)
import Data.Map (lookup)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import React.Basic.DOM as DOM
import React.Basic.Hooks (useContext, useEffect)
import React.Basic.Hooks as React
import State.Helpers (useSelector)
import State.Selectors (todosMapSelector)
import State.Todo (TodoId, genUniqTodo)
import State.TodosMapReducer (changeStatus, addTodo)

type Props
  = { parentId :: TodoId }

mkTodosListPage :: AppComponent Props
mkTodosListPage = do
  { store } <- ask
  todosList <- mkTodosList
  addTodoInput <- mkAddTodoInput
  layout <- mkLayout
  todosListNav <- mkTodosListNav
  appComponent "TodosListPage" \{ parentId } -> React.do
    todosMapState <- useSelector store.stateContext todosMapSelector
    dispatch <- useContext store.dispatchContext
    useEffect parentId do
      log $ "Parent ID: " <> parentId
      mempty
    let
      handleTodoChangeStatus :: TodoId -> Boolean -> Effect Unit
      handleTodoChangeStatus id status = dispatch $ changeStatus id status

      handleAdd :: String -> Effect Unit
      handleAdd text = do
        newTodo <- genUniqTodo parentId text false
        dispatch $ addTodo newTodo

      maybeParent = lookup parentId todosMapState

      maybePrevious = case maybeParent of
        Just parent -> lookup parent.parent todosMapState
        _ -> Nothing

      showedTodos = case maybeParent of
        Just parent -> mapMaybe (\id -> lookup id todosMapState) parent.children
        _ -> fromFoldable []

      computedTodosListNav = case (Tuple maybeParent maybePrevious) of
        (Tuple (Just parent) (Just previous)) ->
          [ todosListNav
              { parentTodo: parent
              , previousTodo: previous
              }
          ]
        _ -> []
    pure
      $ layout
          [ DOM.div
              { className: "pt-40"
              , children:
                  [ DOM.div_ computedTodosListNav
                  , DOM.div
                      { className: "mt-4"
                      , children:
                          [ addTodoInput { onAdd: handleAdd }
                          ]
                      }
                  , DOM.div
                      { className: if length showedTodos > 0 then "mt-4" else ""
                      , children:
                          [ todosList
                              { todos: showedTodos
                              , onTodoChangeStatus: handleTodoChangeStatus
                              }
                          ]
                      }
                  ]
              }
          ]
