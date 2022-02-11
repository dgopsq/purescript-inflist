module App.Pages.NotFound where

import Prelude
import AppEnv (AppComponent, appComponent)
import React.Basic.DOM as DOM

mkNotFound :: AppComponent Unit
mkNotFound = do
  appComponent "About" \_ -> React.do
    pure
      $ DOM.div_
          [ DOM.h1_ [ DOM.text "404 Not Found" ] ]
