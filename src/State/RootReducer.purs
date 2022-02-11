module State.RootReducer where

import Data.Variant (Variant, match)
import State.ToggleReducer as Toggle

type RootAction
  = Variant ( toggle :: Toggle.ToggleAction )

type RootState
  = { toggle :: Toggle.ToggleState }

rootInitialState :: RootState
rootInitialState = { toggle: Toggle.toggleInitialState }

rootReducer :: RootState -> RootAction -> RootState
rootReducer state =
  match
    { toggle: \action -> state { toggle = Toggle.toggleReducer state.toggle action } }
