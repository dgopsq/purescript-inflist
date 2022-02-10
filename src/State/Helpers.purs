module State.Helpers where

import Prelude
import AppEnv (AppComponent, appComponent)
import Control.Monad.Reader (ask, lift)
import Data.Newtype (class Newtype)
import Effect (Effect)
import React.Basic (JSX, ReactContext, provider)
import React.Basic.Hooks (type (&), Hook, UseContext, UseEffect, UseState, coerceHook, createContext, mkReducer, useContext, useEffect, useReducer, useState, (/\))
import React.Basic.Hooks as React
import State.RootReducer (DispatchContext, StateContext, rootInitialState, rootReducer)

mkStateContext :: Effect StateContext
mkStateContext = createContext rootInitialState

mkDispatchContext :: Effect DispatchContext
mkDispatchContext = createContext (\_ -> do pure unit)

mkStateProvider :: AppComponent (Array JSX)
mkStateProvider = do
  { stateContext, dispatchContext } <- ask
  reducer <- lift $ mkReducer rootReducer
  appComponent "State" \children -> React.do
    state /\ dispatch <- useReducer rootInitialState reducer
    let
      stateProvider = provider stateContext

      dispatchProvider = provider dispatchContext
    pure $ dispatchProvider dispatch [ stateProvider state children ]

useSelector :: forall b ctx. Eq ctx => ReactContext ctx -> (ctx -> b) -> Hook (UseSelector ctx b) b
useSelector sc selector =
  coerceHook React.do
    state <- useContext sc
    selectedState /\ setSelectedState <- useState $ selector state
    useEffect state do
      setSelectedState \_ -> selector state
      mempty
    pure selectedState

newtype UseSelector ctx b hooks
  = UseSelector
  ( hooks
      & UseContext ctx
      & UseState b
      & UseEffect ctx
  )

derive instance newtypeUseSelector :: Newtype (UseSelector ctx b hooks) _
