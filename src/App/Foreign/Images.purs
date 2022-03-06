module App.Foreign.Images where

import Prelude
import Data.Function.Uncurried (Fn0, runFn0)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)

foreign import getLogoImage_ :: Fn0 (Nullable String)

getLogoImage :: Maybe String
getLogoImage = toMaybe $ runFn0 getLogoImage_
