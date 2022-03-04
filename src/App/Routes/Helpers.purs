module App.Routes.Helpers where

import Prelude
import App.Routes (AppRoute(..), Router, RouterContext, RouterContextValue, mkAppRoute)
import AppComponent (AppComponent, appComponent)
import Control.Monad.Reader as Reader
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign)
import Partial.Unsafe as Partial.Unsafe
import React.Basic (JSX)
import React.Basic as React.Basic
import React.Basic.Hooks (Hook, UseContext, (/\))
import React.Basic.Hooks as React
import Routing.PushState (PushStateInterface)
import Routing.PushState as PushState

-- | The application's router context.
mkRouterContext :: Effect RouterContext
mkRouterContext = React.createContext Nothing

-- | The `Router` data structure used
-- | in the dependency injection.
mkRouter :: Effect Router
mkRouter = do
  routerContext <- mkRouterContext
  pure { routerContext }

-- | Hook used to retrieve the Router context from
-- | a React component. This must be nested inside
-- | a provider serving the routing dependencies.
useRouterContext ::
  RouterContext ->
  Hook (UseContext (Maybe RouterContextValue)) RouterContextValue
useRouterContext routerContext = React.do
  maybeContextValue <- React.useContext routerContext
  pure case maybeContextValue of
    Nothing -> Partial.Unsafe.unsafeCrashWith "The Provider component for the Router context is missing"
    Just contextValue -> contextValue

-- | Create the Router provider component.
-- | This will initialize the navigation interface
-- | and will provide a `route` state (which is
-- | just a React state updated using the PushState
-- | interface callback).
mkRouterProvider :: Maybe String -> AppComponent (Array JSX)
mkRouterProvider maybeRootDir = do
  { router } <- Reader.ask
  nav <- liftEffect PushState.makeInterface
  appComponent "Router" \children -> React.do
    let
      routerProvider = React.Basic.provider router.routerContext
    route /\ setRoute <- React.useState' (Just RootTodos)
    React.useEffectOnce do
      nav
        # PushState.matches (mkAppRoute maybeRootDir) \_ newRoute -> do
            setRoute newRoute
    pure (routerProvider (Just { nav, route }) children)

-- | A navigation utility used to navigate toward
-- | a specific route using the navigation
-- | interface.
navigateTo :: PushStateInterface -> String -> Effect Unit
navigateTo nav route = nav.pushState (unsafeToForeign {}) route
