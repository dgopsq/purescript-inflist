module State.RootReducer where

import Data.Variant (Variant, match)
import State.ParentTodoReducer as ParentTodo
import State.ParentTodoReducer as ParentTodoReducer
import State.TodosMapReducer as TodosMap
import State.ToggleReducer as Toggle

type RootAction
  = Variant
      ( toggle :: Toggle.ToggleAction
      , todosMap :: TodosMap.TodosMapAction
      , parentTodo :: ParentTodoReducer.ParentTodoAction
      )

type RootState
  = { toggle :: Toggle.ToggleState
    , todosMap :: TodosMap.TodosMapState
    , parentTodo :: ParentTodoReducer.ParentTodoState
    }

rootInitialState :: RootState
rootInitialState =
  { toggle: Toggle.toggleInitialState
  , todosMap: TodosMap.todosMapInitialState
  , parentTodo: ParentTodo.parentTodoInitialState
  }

rootReducer :: RootState -> RootAction -> RootState
rootReducer state =
  match
    { toggle: \action -> state { toggle = Toggle.toggleReducer state.toggle action }
    , todosMap: \action -> state { todosMap = TodosMap.todosMapReducer state.todosMap action }
    , parentTodo: \action -> state { parentTodo = ParentTodo.parentTodoReducer state.parentTodo action }
    }
