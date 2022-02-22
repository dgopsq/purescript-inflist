module App.Foreign.UseClickOutside where

import Prelude
import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import React.Basic (Ref)
import React.Basic.Hooks (Hook, unsafeHook)
import Web.DOM (Node)

-- | The `useClickOutside` hook.
useClickOutside :: Effect Unit -> Hook UseClickOutside (Ref (Nullable Node))
useClickOutside callback =
  unsafeHook do
    runEffectFn1 useClickOutside_ callback

foreign import data UseClickOutside :: Type -> Type

foreign import useClickOutside_ :: EffectFn1 (Effect Unit) (Ref (Nullable Node))
