module App.Foreign.TextareaAutosize where

import Prelude
import React.Basic (JSX, ReactComponent, element)
import React.Basic.Events (EventHandler)
import Untagged.Castable (class Castable, cast)
import Untagged.Union (UndefinedOr)

-- The `UndefinedOr` here is used to allow
-- passing optional props to the component
-- using the `cast` function below.
type Props
  = { key :: UndefinedOr String
    , className :: UndefinedOr String
    , type :: UndefinedOr String
    , placeholder :: UndefinedOr String
    , value :: UndefinedOr String
    , onChange :: UndefinedOr EventHandler
    , onFocus :: UndefinedOr EventHandler
    , onBlur :: UndefinedOr EventHandler
    }

foreign import textareaAutosize_ :: ReactComponent Props

textareaAutosize :: forall r. Castable r Props => r -> JSX
textareaAutosize = element textareaAutosize_ <<< cast
