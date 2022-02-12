module App.Utilities.Todo where

import Prelude
import Data.UUID (genUUID)
import Effect (Effect)
import State.TodosMapReducer (Todo, TodoId)

mkTodoWithoutId :: TodoId -> String -> Boolean -> Todo
mkTodoWithoutId id text checked = { id, text, checked }

mkTodo :: String -> Boolean -> Effect Todo
mkTodo text checked = do
  uuid <- genUUID
  pure $ mkTodoWithoutId (show uuid) text checked
