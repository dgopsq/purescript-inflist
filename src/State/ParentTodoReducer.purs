module State.ParentTodoReducer where

import Prelude
import App.Utilities.Todo (mkTodoWithoutId)
import Data.Variant (Variant, inj)
import State.TodosMapReducer (TodoId, Todo)
import Type.Proxy (Proxy(..))

rootTodo :: Todo
rootTodo = mkTodoWithoutId "__root" "" false

type ParentTodoState
  = TodoId

data ParentTodoAction
  = ChangeParentTodo TodoId

parentTodoInitialState :: ParentTodoState
parentTodoInitialState = rootTodo.id

parentTodoReducer :: ParentTodoState -> ParentTodoAction -> ParentTodoState
parentTodoReducer _ (ChangeParentTodo id) = id

type ParentTodoAction' v
  = ( parentTodo :: ParentTodoAction | v )

injAction :: forall v. ParentTodoAction -> Variant (ParentTodoAction' v)
injAction = inj (Proxy :: Proxy "parentTodo")

changeParentTodo :: forall v. TodoId -> Variant (ParentTodoAction' v)
changeParentTodo = injAction <<< ChangeParentTodo
