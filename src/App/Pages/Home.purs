module App.Pages.Home where

import Prelude
import App.Components.Link (mkLink)
import AppEnv (AppComponent, appComponent)
import Control.Monad.Reader (ask)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (useContext)
import React.Basic.Hooks as React
import State.Helpers (useSelector)
import State.Selectors (toggleSelector)
import State.ToggleReducer (toggle)

mkHome :: AppComponent Unit
mkHome = do
  { store } <- ask
  link <- mkLink
  appComponent "Home" \_ -> React.do
    toggleState <- useSelector store.stateContext toggleSelector
    dispatch <- useContext store.dispatchContext
    let
      handleToggle = dispatch toggle
    pure
      $ DOM.div_
          [ DOM.h1_ [ DOM.text "Home" ]
          , DOM.div_
              [ link { route: "/about", text: "About" } ]
          , DOM.p_ [ DOM.text "Try clicking the button!" ]
          , DOM.button
              { onClick: capture_ do handleToggle
              , children:
                  [ DOM.text "Clicks: "
                  , DOM.text (show toggleState)
                  ]
              }
          ]
