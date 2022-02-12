module App.Pages.TodosListPage where

import Prelude
import App.Components.AddTodoInput (mkAddTodoInput)
import App.Components.TodosList (mkTodosList)
import AppEnv (AppComponent, appComponent)
import Control.Monad.Reader (ask)
import Data.Map (values)
import Effect (Effect)
import Effect.Console (log)
import React.Basic.DOM as DOM
import React.Basic.Hooks (useContext)
import React.Basic.Hooks as React
import State.Helpers (useSelector)
import State.Selectors (todosMapSelector)
import State.TodosMapReducer (TodoId, changeStatus)

mkTodosListPage :: AppComponent Unit
mkTodosListPage = do
  { store } <- ask
  todosList <- mkTodosList
  addTodoInput <- mkAddTodoInput
  appComponent "TodosListPage" \_ -> React.do
    todosMapState <- useSelector store.stateContext todosMapSelector
    dispatch <- useContext store.dispatchContext
    let
      handleTodoChangeStatus :: TodoId -> Boolean -> Effect Unit
      handleTodoChangeStatus id status = dispatch $ changeStatus id status
    pure
      $ DOM.div
          { className: "flex flex-row justify-center pt-48"
          , children:
              [ DOM.div_
                  [ todosList
                      { todos: values todosMapState
                      , onTodoChangeStatus: handleTodoChangeStatus
                      }
                  , DOM.div_
                      [ addTodoInput { onAdd: log } ]
                  ]
              ]
          }
