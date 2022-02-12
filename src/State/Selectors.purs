module State.Selectors where

import State.RootReducer (RootState)
import State.TodosMapReducer (TodosMapState)
import State.ToggleReducer (ToggleState)

toggleSelector :: RootState -> ToggleState
toggleSelector state = state.toggle

todosMapSelector :: RootState -> TodosMapState
todosMapSelector state = state.todosMap
