module App.Misc.Codecs where

import Data.Argonaut (Json, JsonDecodeError, decodeJson, encodeJson)
import Data.Either (Either)
import App.State.Todo (Todo)

-- | Codec used to encode a Todo into
-- | a JSON object.
todoToJson :: Todo -> Json
todoToJson = encodeJson

-- | Codec used to parse a Todo from a
-- | JSON object.
todoFromJson :: Json -> Either JsonDecodeError Todo
todoFromJson = decodeJson
