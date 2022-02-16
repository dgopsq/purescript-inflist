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
  | UpdateTodo TodoId Todo
  | LoadTodo Todo

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

todosMapReducer state (UpdateTodo todoId newTodo) = case maybeTodo of
  Just _ -> insert todoId newTodo state
  Nothing -> state
  where
  maybeTodo = lookup todoId state

todosMapReducer state (LoadTodo todo) = insert todo.id todo state

type TodosMapAction' v
  = ( todosMap :: TodosMapAction | v )

injAction :: forall v. TodosMapAction -> Variant (TodosMapAction' v)
injAction = inj (Proxy :: Proxy "todosMap")

addTodo :: forall v. Todo -> Variant (TodosMapAction' v)
addTodo = injAction <<< AddTodo

updateTodo :: forall v. TodoId -> Todo -> Variant (TodosMapAction' v)
updateTodo todoId newTodo = injAction (UpdateTodo todoId newTodo)

loadTodo :: forall v. Todo -> Variant (TodosMapAction' v)
loadTodo = injAction <<< LoadTodo
