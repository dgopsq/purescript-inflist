module App.Components.Layout where

import Prelude
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component)

type Props
  = Array JSX

mkLayout :: Component Props
mkLayout =
  component "Layout" \children -> React.do
    pure
      $ DOM.div
          { className: "flex flex-row justify-center"
          , children:
              [ DOM.div
                  { className: "max-w-4xl w-full px-8"
                  , children
                  }
              ]
          }
