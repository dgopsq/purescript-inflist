module App.Foreign.UseContextSelector where

import Prelude
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import React.Basic.Hooks (Hook, ReactContext, unsafeHook)

-- | The `useContextSelector` hook.
useContextSelector ::
  forall b ctx.
  Eq ctx =>
  ReactContext ctx ->
  (ctx -> b) ->
  Hook (UseContextSelector ctx b) b
useContextSelector context selector =
  unsafeHook do
    runEffectFn2 useContextSelector_ context selector

foreign import data UseContextSelector :: Type -> Type -> Type -> Type

foreign import useContextSelector_ ::
  forall b ctx.
  EffectFn2 (ReactContext ctx) (ctx -> b) b

-- | ...
foreign import createContextSelector :: forall a. a -> Effect (ReactContext a)
