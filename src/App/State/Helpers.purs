module App.State.Helpers where

import Prelude
import App.Foreign.UseContextSelector (UseContextSelector, useContextSelector)
import App.State.RootReducer (RootState, RootAction)
import AppComponent (AppComponent, appComponent)
import Control.Monad.Reader (ask, lift)
import Data.Newtype (class Newtype)
import React.Basic (JSX, ReactContext, provider)
import React.Basic.Hooks (type (&), Hook, coerceHook, mkReducer, useReducer, (/\))
import React.Basic.Hooks as React

-- | Utility function to generate a provider component
-- | for the app state. This provider will add both
-- | the state context and the dispatch context.
mkStoreProvider :: RootState -> (RootState -> RootAction -> RootState) -> AppComponent (Array JSX)
mkStoreProvider initialState rootReducer = do
  { store } <- ask
  reducer <- lift $ mkReducer rootReducer
  appComponent "State" \children -> React.do
    state /\ dispatch <- useReducer initialState reducer
    let
      stateProvider = provider store.stateContext

      dispatchProvider = provider store.dispatchContext
    pure $ dispatchProvider dispatch [ stateProvider state children ]

-- | Hook that simulates the `useSelector` from Redux.
-- | This will take the state context and a selector function
-- | to subscribe only to a specific part of the state.
useSelector :: forall b ctx. Eq ctx => ReactContext ctx -> (ctx -> b) -> Hook (UseSelector ctx b) b
useSelector sc selector =
  coerceHook React.do
    state <- useContextSelector sc selector
    pure state

newtype UseSelector ctx b hooks
  = UseSelector
  ( hooks
      & UseContextSelector ctx b
  )

derive instance newtypeUseSelector :: Newtype (UseSelector ctx b hooks) _
