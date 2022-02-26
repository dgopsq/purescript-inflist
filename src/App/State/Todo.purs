module App.State.Todo where

import Prelude
import Data.List (List(..))
import Data.Tuple (Tuple(..))
import Data.UUID (genUUID, toString)
import Effect (Effect)

-- | A Todo id. This should be a unique identificator
-- | for a single Todo.
type TodoId
  = String

-- | A data structure representing a Todo.
-- | This is the primary atomic element of this application. 
-- | A Todo is a structure with:
-- |
-- |  1.  `id`: A unique identifier.
-- |  2.  `checked`: A boolean state representing whether the Todo
-- |      has been completed or not.
-- |  3.  `text`: The Todo description.
-- |  4.  `children`: A Todo could have an infinite number of Children
-- |      which are just a list of TodoId.
-- |  5.  `parent`: A Todo has a single Parent. If the Todo is at the
-- |      top level then its parent will be `rootTodoId` which is an
-- |      hardcoded "Super Todo" sitting at the application's top level.
type Todo
  = { id :: TodoId
    , checked :: Boolean
    , text :: String
    , children :: List TodoId
    , parent :: TodoId
    }

-- | Helper function used to create a Todo.
mkTodo :: TodoId -> TodoId -> String -> Boolean -> Todo
mkTodo parentId id text checked = { id, text, checked, parent: parentId, children }
  where
  children = Nil

-- | Helper function used to generate a Todo with
-- | a unique id.
genUniqTodo :: TodoId -> String -> Boolean -> Effect Todo
genUniqTodo parentId text checked = do
  uuid <- genUUID
  pure $ mkTodo parentId (toString uuid) text checked

-- | The TodoId of the application's top level Todo.
rootTodoId :: TodoId
rootTodoId = "__root__"

-- | The application's top level todo. This is the "root"
-- | element which is the Parent of all the other Todos.
rootTodo :: Todo
rootTodo = mkTodo rootTodoId rootTodoId "" false

-- | Helper Tuple with the `rootTodoId` and the `rootTodo`.
rootTodoTuple :: Tuple TodoId Todo
rootTodoTuple = Tuple rootTodoId rootTodo

-- | Helper function which check if a Todo is
-- | the root Todo.
isRootTodo :: Todo -> Boolean
isRootTodo todo = todo.id == todo.parent
