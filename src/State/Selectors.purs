module State.Selectors where

import State.ParentTodoReducer (ParentTodoState)
import State.RootReducer (RootState)
import State.TodosMapReducer (TodosMapState)
import State.ToggleReducer (ToggleState)

toggleSelector :: RootState -> ToggleState
toggleSelector state = state.toggle

todosMapSelector :: RootState -> TodosMapState
todosMapSelector state = state.todosMap

parentTodoSelector :: RootState -> ParentTodoState
parentTodoSelector state = state.parentTodo
