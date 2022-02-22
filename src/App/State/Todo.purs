module App.State.Todo where

import Prelude
import Data.List (List, fromFoldable)
import Data.Tuple (Tuple(..))
import Data.UUID (genUUID, toString)
import Effect (Effect)

type TodoId
  = String

type Todo
  = { id :: TodoId
    , checked :: Boolean
    , text :: String
    , children :: List TodoId
    , parent :: TodoId
    }

mkTodo :: TodoId -> TodoId -> String -> Boolean -> Todo
mkTodo parentId id text checked = { id, text, checked, parent: parentId, children }
  where
  children = fromFoldable []

genUniqTodo :: TodoId -> String -> Boolean -> Effect Todo
genUniqTodo parentId text checked = do
  uuid <- genUUID
  pure $ mkTodo parentId (toString uuid) text checked

rootTodoId :: TodoId
rootTodoId = "__root__"

rootTodo :: Todo
rootTodo = mkTodo rootTodoId rootTodoId "" false

rootTodoTuple :: Tuple TodoId Todo
rootTodoTuple = Tuple rootTodoId rootTodo

isRootTodo :: Todo -> Boolean
isRootTodo todo = todo.id == todo.parent
