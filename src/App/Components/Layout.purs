module App.Components.Layout where

import Prelude
import AppEnv (AppComponent, appComponent)
import React.Basic (JSX)
import React.Basic.DOM as DOM

type Props
  = Array JSX

mkLayout :: AppComponent Props
mkLayout =
  appComponent "Layout" \children -> React.do
    pure
      $ DOM.div
          { className: "flex flex-row justify-center"
          , children:
              [ DOM.div
                  { className: "max-w-xl w-full px-8"
                  , children
                  }
              ]
          }
