module App.Pages.Home where

import Prelude

import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (Component, component, useState, (/\))
import React.Basic.Hooks as React

type HomeProps = Unit

mkHome :: Component HomeProps
mkHome = do
  component "Home" \_props -> React.do

    counter /\ setCounter <- useState 0

    pure $ DOM.div
      { children:
          [ DOM.h1_ [ DOM.text "Home" ]
          , DOM.p_ [ DOM.text "Try clicking the button!" ]
          , DOM.button
              { onClick: capture_ do
                  setCounter (_ + 1)
              , children:
                  [ DOM.text "Clicks: "
                  , DOM.text (show counter)
                  ]
              }
          ]
      }