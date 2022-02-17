module Misc.Hook.UsePrev where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Effect.Class (liftEffect)
import React.Basic.Hooks (type (&), Hook, UseEffect, UseRef, coerceHook, readRef, useEffect, useRef, writeRef)
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (UseAff, useAff)

usePrev :: forall b. Eq b => b -> Hook (UsePrev b) (Maybe b)
usePrev value =
  coerceHook React.do
    ref <- useRef Nothing
    useEffect value do
      writeRef ref (Just value)
      mempty
    current <- useAff value $ liftEffect (readRef ref)
    pure $ fromMaybe Nothing current

newtype UsePrev b hooks
  = UsePrev
  ( hooks
      & UseRef (Maybe b)
      & UseEffect b
      & UseAff b (Maybe b)
  )

derive instance newtypeUsePrev :: Newtype (UsePrev b hooks) _
