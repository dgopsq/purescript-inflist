module State.TodosMapReducer where

import Prelude
import Data.List (snoc)
import Data.Map (Map, fromFoldable, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.Variant (Variant, inj)
import State.Todo (Todo, TodoId, rootTodoTuple)
import Type.Proxy (Proxy(..))

type TodosMapState
  = Map TodoId Todo

data TodosMapAction
  = AddTodo Todo
  | ChangeStatus TodoId Boolean

todosMapInitialState :: TodosMapState
todosMapInitialState = fromFoldable [ rootTodoTuple ]

todosMapReducer :: TodosMapState -> TodosMapAction -> TodosMapState
todosMapReducer state (AddTodo todo)
  | length todo.text > 0 = case maybeParent of
    Just parent ->
      insert parent.id
        ( parent
            { children = snoc parent.children todo.id
            }
        )
        stateWithAddedTodo
    _ -> state
    where
    maybeParent = lookup todo.parent state

    stateWithAddedTodo = insert todo.id todo state
  | otherwise = state

todosMapReducer state (ChangeStatus todoId status) = case maybeTodo of
  Just todo -> insert todoId (todo { checked = status }) state
  Nothing -> state
  where
  maybeTodo = lookup todoId state

type TodosMapAction' v
  = ( todosMap :: TodosMapAction | v )

injAction :: forall v. TodosMapAction -> Variant (TodosMapAction' v)
injAction = inj (Proxy :: Proxy "todosMap")

addTodo :: forall v. Todo -> Variant (TodosMapAction' v)
addTodo = injAction <<< AddTodo

changeStatus :: forall v. TodoId -> Boolean -> Variant (TodosMapAction' v)
changeStatus todoId status = injAction (ChangeStatus todoId status)
