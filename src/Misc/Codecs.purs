module Misc.Codecs where

import Data.Argonaut (Json, JsonDecodeError, decodeJson, encodeJson)
import Data.Either (Either)
import State.Todo (Todo)

todoToJson :: Todo -> Json
todoToJson = encodeJson

todoFromJson :: Json -> Either JsonDecodeError Todo
todoFromJson = decodeJson
