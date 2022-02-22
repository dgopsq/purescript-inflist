module App.State.Selectors where

import App.State.RootReducer (RootState)
import App.State.TodosMapReducer (TodosMapState)
import App.State.ToggleReducer (ToggleState)

toggleSelector :: RootState -> ToggleState
toggleSelector state = state.toggle

todosMapSelector :: RootState -> TodosMapState
todosMapSelector state = state.todosMap
