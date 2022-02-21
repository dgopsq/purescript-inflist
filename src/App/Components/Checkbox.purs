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

mkCheckbox :: Component Props
mkCheckbox = do
  component "Checkbox" \{ checked, onChange } -> React.do
    pure
      $ DOM.input
          { className: "appearance-none checked:bg-emerald-500 bg-slate-200 w-4 h-4 rounded cursor-pointer"
          , type: "checkbox"
          , checked
          , onChange: mkEffectFn1 \_ -> onChange $ not checked
          }
