module App.State.Store where

import Prelude
import App.Foreign.UseContextSelector (createContextSelector)
import Effect (Effect)
import React.Basic (ReactContext, createContext)
import App.State.RootReducer (RootAction, RootState, rootInitialState)

-- | The type describing a React context that contains
-- | the application's state.
type StateContext
  = ReactContext RootState

-- | The type describing a React context that contains
-- | the dispatch function to send actions to the state.
type DispatchContext
  = ReactContext (RootAction -> Effect Unit)

-- | Data structure describing the whole application's store.
-- | The store is just a Record containing the application's state
-- | and the dispatch function.
type Store
  = { stateContext :: StateContext
    , dispatchContext :: DispatchContext
    }

-- | The application state's context.
mkStateContext :: Effect StateContext
mkStateContext = createContextSelector rootInitialState

-- | The application's dispatch context.
mkDispatchContext :: Effect DispatchContext
mkDispatchContext = createContext (\_ -> do pure unit)

-- | The application's store created aggregating
-- | together the application's state and the dispatch
-- | function inside a single Record.
mkStore :: Effect Store
mkStore = do
  stateContext <- mkStateContext
  dispatchContext <- mkDispatchContext
  pure { stateContext, dispatchContext }
