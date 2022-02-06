module App.Pages.About where

import Prelude
import App.Components.Link (mkLink)
import AppEnv (AppComponent, appComponent)
import React.Basic.DOM as DOM

mkAbout :: AppComponent Unit
mkAbout = do
  link <- mkLink
  appComponent "About" \_ -> React.do
    pure
      $ DOM.div_
          [ DOM.h1_ [ DOM.text "About" ]
          , DOM.div_
              [ link { route: "/", text: "Home" } ]
          ]
