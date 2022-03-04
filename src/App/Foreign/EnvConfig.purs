module App.Foreign.EnvConfig
  ( getRootDir
  ) where

import Prelude
import Data.Function.Uncurried (Fn0, runFn0)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)

foreign import getRootDir_ :: Fn0 (Nullable String)

getRootDir :: Maybe String
getRootDir = toMaybe $ runFn0 getRootDir_
