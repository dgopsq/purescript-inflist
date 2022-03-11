module App.Components.Checkbox where

import Prelude
import Effect (Effect)
import Foreign.Object as FO
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Events (EventHandler)
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
  component "Checkbox" \{ checked, onChange } -> React.do
    let
      handleChange :: EventHandler
      handleChange = capture_ <<< onChange $ not checked
    pure case checked of
      true ->
        DOM.div
          { className: "rounded-md block bg-black w-5 h-5 text-white cursor-pointer"
          , role: "checkbox"
          , tabIndex: 0
          , _aria: FO.singleton "checked" "true"
          , onClick: handleChange
          , children:
              [ DOM.i
                  { className: "bg-black w-full h-full rounded-md gg-check gg-check-fix"
                  , children: []
                  }
              ]
          }
      false ->
        DOM.div
          { className: "rounded-md block bg-gray-300 w-5 h-5 cursor-pointer"
          , role: "checkbox"
          , tabIndex: 0
          , _aria: FO.singleton "checked" "false"
          , onClick: handleChange
          , children: []
          }
