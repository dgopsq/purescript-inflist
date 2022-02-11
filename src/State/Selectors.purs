module State.Selectors where

import State.RootReducer (RootState)
import State.ToggleReducer (ToggleState)

toggleSelector :: RootState -> ToggleState
toggleSelector state = state.toggle
