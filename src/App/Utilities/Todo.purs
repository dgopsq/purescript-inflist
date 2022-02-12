module App.Utilities.Todo where

import Prelude
import Data.UUID (genUUID)
import Effect (Effect)
import State.TodosMapReducer (Todo)

mkTodo :: String -> Boolean -> Effect Todo
mkTodo text checked = do
  uuid <- genUUID
  pure { id: show uuid, text, checked }
