module State.ParentTodoReducer where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import State.Todo (Todo, TodoId, mkTodo)
import Type.Proxy (Proxy(..))

rootTodoId :: TodoId
rootTodoId = "__root__"

rootTodo :: Todo
rootTodo = mkTodo rootTodoId rootTodoId "" false

rootTodoTuple :: Tuple TodoId Todo
rootTodoTuple = Tuple rootTodoId rootTodo

type ParentTodoState
  = TodoId

data ParentTodoAction
  = ChangeParentTodo TodoId

parentTodoInitialState :: ParentTodoState
parentTodoInitialState = rootTodoId

parentTodoReducer :: ParentTodoState -> ParentTodoAction -> ParentTodoState
parentTodoReducer _ (ChangeParentTodo id) = id

type ParentTodoAction' v
  = ( parentTodo :: ParentTodoAction | v )

injAction :: forall v. ParentTodoAction -> Variant (ParentTodoAction' v)
injAction = inj (Proxy :: Proxy "parentTodo")

changeParentTodo :: forall v. TodoId -> Variant (ParentTodoAction' v)
changeParentTodo = injAction <<< ChangeParentTodo
