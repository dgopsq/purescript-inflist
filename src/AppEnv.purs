module AppEnv where

import Prelude
import State.RootReducer (StateContext, DispatchContext)
import Control.Monad.Reader (ReaderT(..))
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.Hooks (Render)
import React.Basic.Hooks as React
import Routes (RouterContext)

-- | The Record used for the DI.
type AppEnv
  = { routerContext :: RouterContext
    , stateContext :: StateContext
    , dispatchContext :: DispatchContext
    }

-- | Wrap the default `React.Basic.Hooks.Component` using the
-- | `ReaderT` monad transformer for Dependency Injection.
type AppComponent props
  = ReaderT AppEnv Effect (props -> JSX)

-- | Create a `React.Basic.Hooks.Component` wrapped into
-- | a `ReaderT` monad transformer for Dependency Injection.
appComponent :: forall props hooks. String -> (props -> Render Unit hooks JSX) -> AppComponent props
appComponent name render = ReaderT \_ -> React.component name render
