module AppComponent where

import Prelude
import App.Api.Storage.Storage (TodosStorage)
import Control.Monad.Reader (ReaderT(..))
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.Hooks (Render)
import React.Basic.Hooks as React
import App.Routes (Router)
import App.State.Store (Store)

-- | The Record used for the DI.
type AppComponentEnv
  = { router :: Router
    , store :: Store
    , todosStorage :: TodosStorage
    }

-- | Wrap the default `React.Basic.Hooks.Component` using the
-- | `ReaderT` monad transformer for Dependency Injection.
type AppComponent props
  = ReaderT AppComponentEnv Effect (props -> JSX)

-- | Create a `React.Basic.Hooks.Component` wrapped into
-- | a `ReaderT` monad transformer for Dependency Injection.
appComponent :: forall props hooks. String -> (props -> Render Unit hooks JSX) -> AppComponent props
appComponent name render = ReaderT \_ -> React.component name render
