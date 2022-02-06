module State.Helpers where

import Prelude
import State.RootReducer (StateContext, DispatchContext, rootInitialState, rootReducer)
import AppEnv (AppComponent, appComponent)
import Control.Monad.Reader (ask, lift)
import Effect (Effect)
import React.Basic (JSX, provider)
import React.Basic.Hooks (mkReducer, useReducer, (/\))
import React.Basic.Hooks as React

mkStateContext :: Effect StateContext
mkStateContext = React.createContext rootInitialState

mkDispatchContext :: Effect DispatchContext
mkDispatchContext = React.createContext (\_ -> do pure unit)

mkStateProvider :: AppComponent (Array JSX)
mkStateProvider = do
  { stateContext, dispatchContext } <- ask
  reducer <- lift $ mkReducer rootReducer
  appComponent "State" \children -> React.do
    state /\ dispatch <- useReducer rootInitialState reducer
    let
      stateProvider = provider stateContext

      dispatchProvider = provider dispatchContext
    pure $ stateProvider state [ dispatchProvider dispatch children ]
