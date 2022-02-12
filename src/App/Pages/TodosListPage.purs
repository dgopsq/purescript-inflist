module App.Pages.TodosListPage where

import Prelude
import App.Components.AddTodoInput (mkAddTodoInput)
import App.Components.TodosList (mkTodosList)
import AppEnv (AppComponent, appComponent)
import Control.Monad.Reader (ask)
import Data.List (List, fromFoldable, length, mapMaybe)
import Data.Map (lookup)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import React.Basic.DOM as DOM
import React.Basic.Hooks (useContext)
import React.Basic.Hooks as React
import State.Helpers (useSelector)
import State.Selectors (todosMapSelector, parentTodoSelector)
import State.Todo (TodoId, Todo, genUniqTodo)
import State.TodosMapReducer (changeStatus, addTodo)

mkTodosListPage :: AppComponent Unit
mkTodosListPage = do
  { store } <- ask
  todosList <- mkTodosList
  addTodoInput <- mkAddTodoInput
  appComponent "TodosListPage" \_ -> React.do
    todosMapState <- useSelector store.stateContext todosMapSelector
    parentTodoState <- useSelector store.stateContext parentTodoSelector
    dispatch <- useContext store.dispatchContext
    let
      handleTodoChangeStatus :: TodoId -> Boolean -> Effect Unit
      handleTodoChangeStatus id status = dispatch $ changeStatus id status

      handleAdd :: String -> Effect Unit
      handleAdd text = do
        newTodo <- genUniqTodo parentTodoState text false
        dispatch $ addTodo newTodo

      showedTodos :: List Todo
      showedTodos = case maybeParent of
        Just parent -> mapMaybe (\id -> lookup id todosMapState) parent.children
        _ -> fromFoldable []
        where
        maybeParent = lookup parentTodoState todosMapState
    pure
      $ DOM.div
          { className: "flex flex-row justify-center pt-48"
          , children:
              [ DOM.div_
                  [ todosList
                      { todos: showedTodos
                      , onTodoChangeStatus: handleTodoChangeStatus
                      }
                  , DOM.div
                      { className: if length showedTodos > 0 then "mt-4" else ""
                      , children:
                          [ addTodoInput { onAdd: handleAdd }
                          ]
                      }
                  ]
              ]
          }
