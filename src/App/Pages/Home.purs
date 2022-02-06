module App.Pages.Home where

import Prelude
import App.Components.Link (mkLink)
import State.RootReducer (RootAction(..))
import AppEnv (AppComponent, appComponent)
import Control.Monad.Reader (ask)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (useContext)
import React.Basic.Hooks as React

mkHome :: AppComponent Unit
mkHome = do
  { stateContext, dispatchContext } <- ask
  link <- mkLink
  appComponent "Home" \_ -> React.do
    state <- useContext stateContext
    dispatch <- useContext dispatchContext
    let
      handleToggle = dispatch Toggle
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
                  , DOM.text (show state)
                  ]
              }
          ]
