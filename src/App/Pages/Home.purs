module App.Pages.Home where

import Prelude
import AppEnv (AppComponent, appComponent)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (useState, (/\))
import React.Basic.Hooks as React

mkHome :: AppComponent Unit
mkHome = do
  appComponent "Home" \_ -> React.do
    counter /\ setCounter <- useState true
    pure
      $ DOM.div
          { children:
              [ DOM.h1_ [ DOM.text "Home" ]
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
          }
