module App.Components.Checkbox where

import Prelude
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component)

type Props
  = { checked :: Boolean
    , onChange :: Boolean -> Effect Unit
    }

-- | A purely visual component rendering a 
-- | simple managed checkbox element.
mkCheckbox :: Component Props
mkCheckbox = do
  component "Checkbox" \{ checked, onChange } -> React.do
    pure
      $ DOM.input
          { className: "appearance-none checked:bg-black bg-gray-300 w-4 h-4 rounded cursor-pointer outline-indigo-300"
          , type: "checkbox"
          , checked
          , onChange: mkEffectFn1 \_ -> onChange $ not checked
          }
