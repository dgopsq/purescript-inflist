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

-- | This is the data structure holding all the 
-- | application's dependencies.
type AppComponentEnv
  = { router :: Router
    , store :: Store
    , todosStorage :: TodosStorage
    }

-- | A type wrapping the default `React.Basic.Hooks.Component` 
-- | using the `ReaderT` monad transformer.
-- | The `Reader` monad is the one used to handle the dependencies 
-- | around the application.
type AppComponent props
  = ReaderT AppComponentEnv Effect (props -> JSX)

-- | Through this function it will be possible to create
-- | components that will be able to interact with the
-- | `Reader` monad in order to retrieve dependencies.
appComponent :: forall props hooks. String -> (props -> Render Unit hooks JSX) -> AppComponent props
appComponent name render = ReaderT \_ -> React.component name render
