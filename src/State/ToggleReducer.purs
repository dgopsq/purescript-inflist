module State.ToggleReducer where

import Prelude
import Data.Variant (Variant, inj)
import Type.Proxy (Proxy(..))

type ToggleState
  = Boolean

data ToggleAction
  = Toggle

toggleInitialState :: ToggleState
toggleInitialState = false

toggleReducer :: ToggleState -> ToggleAction -> ToggleState
toggleReducer state Toggle = not state

type ToggleAction' v
  = ( toggle :: ToggleAction | v )

injAction :: forall v. ToggleAction -> Variant (ToggleAction' v)
injAction = inj (Proxy :: Proxy "toggle")

toggle :: forall v. Variant (ToggleAction' v)
toggle = injAction Toggle
