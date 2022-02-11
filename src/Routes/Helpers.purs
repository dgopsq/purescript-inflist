module Routes.Helpers where

import Prelude
import AppEnv (AppComponent, appComponent)
import Control.Monad.Reader as Reader
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Partial.Unsafe as Partial.Unsafe
import React.Basic (JSX)
import React.Basic as React.Basic
import React.Basic.Hooks (Hook, UseContext, (/\))
import React.Basic.Hooks as React
import Routes (AppRoute(..), RouterContext, RouterContextValue, Router, appRoute)
import Routing.PushState as PushState

-- | Initialize the Router context.
mkRouterContext :: Effect RouterContext
mkRouterContext = React.createContext Nothing

-- | Create the Router object for the DI.
mkRouter :: Effect Router
mkRouter = do
  routerContext <- mkRouterContext
  pure { routerContext }

-- | Hook used to retrieve the Router context from
-- | a React component.
useRouterContext ::
  RouterContext ->
  Hook (UseContext (Maybe RouterContextValue)) RouterContextValue
useRouterContext routerContext = React.do
  maybeContextValue <- React.useContext routerContext
  pure case maybeContextValue of
    -- If we have no context value from a provider, we throw a fatal error
    Nothing ->
      Partial.Unsafe.unsafeCrashWith
        "useContext can only be used in a descendant of the corresponding context provider component"
    Just contextValue -> contextValue

-- | Create the Router provider component.
mkRouterProvider :: AppComponent (Array JSX)
mkRouterProvider = do
  { router } <- Reader.ask
  nav <- liftEffect PushState.makeInterface
  appComponent "Router" \children -> React.do
    let
      routerProvider = React.Basic.provider router.routerContext
    route /\ setRoute <- React.useState' (Just Home)
    React.useEffectOnce do
      nav
        # PushState.matches appRoute \_ newRoute -> do
            setRoute newRoute
    pure (routerProvider (Just { nav, route }) children)
