module State.Store where

import Prelude
import App.Foreign.UseContextSelector (createContextSelector)
import Effect (Effect)
import React.Basic (ReactContext, createContext)
import State.RootReducer (RootAction, RootState, rootInitialState)

type StateContext
  = ReactContext RootState

type DispatchContext
  = ReactContext (RootAction -> Effect Unit)

type Store
  = { stateContext :: StateContext
    , dispatchContext :: DispatchContext
    }

mkStateContext :: Effect StateContext
mkStateContext = createContextSelector rootInitialState

mkDispatchContext :: Effect DispatchContext
mkDispatchContext = createContext (\_ -> do pure unit)

mkStore :: Effect Store
mkStore = do
  stateContext <- mkStateContext
  dispatchContext <- mkDispatchContext
  pure { stateContext, dispatchContext }
