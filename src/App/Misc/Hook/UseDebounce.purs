module App.Misc.Hook.UseDebouncedEffect where

import Prelude
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Ref (modify, new, read)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Timer (clearTimeout, setTimeout)
import React.Basic.Hooks (type (&), Hook, UseEffect, UseState, coerceHook, useEffect, useState, (/\))
import React.Basic.Hooks as React

-- | Hook used to debounce a value's update. This
-- | will assure that a specific value will be
-- | updated only once in a specific predetermined
-- | timeframe.
-- | 
-- | A concrete usage of this hook can be found in the
-- | `Todo` component, which will trigger a 
-- | callback function everytime a text input has
-- | changed. In this case `useDebounce` will assure that
-- | the callback function won't be called too many times
-- | but only when the used has finished writing.
useDebounce :: forall b. Eq b => Int -> b -> Hook (UseDebounce Int b) b
useDebounce timeout value =
  coerceHook React.do
    debouncedValue /\ setDebouncedValue <- useState value
    useEffect { value, timeout } do
      ref <- liftST $ new Nothing
      id <- setTimeout timeout $ setDebouncedValue \_ -> value
      _ <- liftST $ modify (\_ -> Just id) ref
      pure do
        maybeId <- liftST $ read ref
        case maybeId of
          Just retrievedId -> clearTimeout retrievedId
          _ -> pure unit
    pure debouncedValue

newtype UseDebounce timeout b hooks
  = UseDebounce
  ( hooks
      & UseState b
      & UseEffect { value :: b, timeout :: timeout }
  )

derive instance newtypeUseDebounce :: Newtype (UseDebounce timeout b hooks) _
