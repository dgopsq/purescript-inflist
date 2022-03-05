module App.Components.Checkbox where

import Prelude
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component)

type Props
  = { id :: String
    , checked :: Boolean
    , onChange :: Boolean -> Effect Unit
    }

-- | A purely visual component rendering a 
-- | simple managed checkbox element.
mkCheckbox :: Component Props
mkCheckbox = do
  component "Checkbox" \{ id, checked, onChange } -> React.do
    pure
      $ DOM.div_
          [ DOM.input
              { className: "opacity-0 absolute c-checkbox"
              , type: "checkbox"
              , checked
              , onChange: mkEffectFn1 \_ -> onChange $ not checked
              , id
              }
          , DOM.label
              { className: "rounded-md block bg-gray-300 w-5 h-5 cursor-pointer"
              , htmlFor: id
              , children:
                  [ DOM.i
                      { className: "hidden bg-black w-full h-full rounded-md gg-check gg-check-fix"
                      , children: []
                      }
                  ]
              }
          ]
