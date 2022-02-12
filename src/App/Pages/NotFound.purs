module App.Pages.NotFoundPage where

import Prelude
import AppEnv (AppComponent, appComponent)
import React.Basic.DOM as DOM

mkNotFoundPage :: AppComponent Unit
mkNotFoundPage = do
  appComponent "NotFoundPage" \_ -> React.do
    pure
      $ DOM.div_
          [ DOM.h1_ [ DOM.text "404 Not Found" ] ]
