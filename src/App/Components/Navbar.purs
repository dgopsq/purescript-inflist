module App.Components.Navbar where

import Prelude
import App.Components.Layout (mkLayout)
import App.Components.Link (mkLink)
import App.Foreign.Images (getLogoImage)
import App.State.Todo (TodoId)
import AppComponent (AppComponent, appComponent)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromMaybe)
import React.Basic.DOM as DOM

type Props
  = { parentId :: TodoId }

-- | A purely visual component rendering the 
-- | application's top navbar.
mkNavbar :: AppComponent Props
mkNavbar = do
  layout <- lift mkLayout
  link <- mkLink
  appComponent "Navbar" \_ -> React.do
    pure
      $ DOM.div
          { className: "pt-20"
          , children:
              [ layout
                  [ DOM.div
                      { className: "flex flex-row justify-between items-center"
                      , children:
                          [ DOM.div_
                              [ link
                                  { route: "/"
                                  , children:
                                      [ DOM.span
                                          { className: "flex items-center"
                                          , children:
                                              [ DOM.img
                                                  { className: "h-6 md:h-8 lg:h-10"
                                                  , src: fromMaybe "" getLogoImage
                                                  , alt: "Logo"
                                                  }
                                              , DOM.span
                                                  { className: "text-2xl md:text-3xl lg:text-5xl tracking-wide font-black text-black pl-4"
                                                  , children: [ DOM.text "inflist" ]
                                                  }
                                              ]
                                          }
                                      ]
                                  }
                              ]
                          , DOM.div_
                              [ DOM.a
                                  { className: "text-black flex items-center"
                                  , href: "https://github.com/dgopsq/inflist"
                                  , target: "_blank"
                                  , rel: "noopener"
                                  , children:
                                      [ DOM.i
                                          { className: "gg-code-slash gg-small mr-4"
                                          , children: []
                                          }
                                      , DOM.text "GitHub"
                                      ]
                                  }
                              ]
                          ]
                      }
                  ]
              ]
          }
