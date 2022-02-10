module State.RootReducer where

import Prelude
import Data.Variant (Variant, match)
import Effect (Effect)
import React.Basic.Hooks (ReactContext)
import State.ToggleReducer as Toggle

type RootAction
  = Variant ( toggle :: Toggle.ToggleAction )

type RootState
  = { toggle :: Toggle.ToggleState }

type StateContext
  = ReactContext RootState

type DispatchContext
  = ReactContext (RootAction -> Effect Unit)

rootInitialState :: RootState
rootInitialState = { toggle: Toggle.toggleInitialState }

rootReducer :: RootState -> RootAction -> RootState
rootReducer state =
  match
    { toggle: \action -> state { toggle = Toggle.toggleReducer state.toggle action } }
