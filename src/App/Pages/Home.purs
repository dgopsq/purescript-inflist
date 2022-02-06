module App.Pages.Home where

import Prelude
import App.Components.Link (mkLink)
import AppEnv (AppComponent, appComponent)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (useState, (/\))
import React.Basic.Hooks as React

mkHome :: AppComponent Unit
mkHome = do
  link <- mkLink
  appComponent "Home" \_ -> React.do
    counter /\ setCounter <- useState true
    pure
      $ DOM.div_
          [ DOM.h1_ [ DOM.text "Home" ]
          , DOM.div_
              [ link { route: "/about", text: "About" } ]
          , DOM.p_ [ DOM.text "Try clicking the button!" ]
          , DOM.button
              { onClick:
                  capture_ do
                    setCounter $ not
              , children:
                  [ DOM.text "Clicks: "
                  , DOM.text (show counter)
                  ]
              }
          ]
