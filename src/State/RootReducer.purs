module State.RootReducer where

import Prelude
import Effect (Effect)
import React.Basic.Hooks (ReactContext)

data RootAction
  = Toggle

type RootState
  = Boolean

type StateContext
  = ReactContext RootState

type DispatchContext
  = ReactContext (RootAction -> Effect Unit)

rootInitialState :: RootState
rootInitialState = true

rootReducer :: RootState -> RootAction -> RootState
rootReducer state Toggle = not state
