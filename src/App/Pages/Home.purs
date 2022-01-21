module App.Pages.Home where

import Prelude

import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (Component, component, useState, (/\))
import React.Basic.Hooks as React

type Props = Unit

mkHome :: Component Props
mkHome = do
  component "Home" \_ -> React.do

    counter /\ setCounter <- useState true

    pure $ DOM.div
      { children:
          [ DOM.h1_ [ DOM.text "Home" ]
          , DOM.p_ [ DOM.text "Try clicking the button!" ]
          , DOM.button
              { onClick: capture_ do
                  setCounter $ not
              , children:
                  [ DOM.text "Clicks: "
                  , DOM.text (show counter)
                  ]
              }
          ]
      }