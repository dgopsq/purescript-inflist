module App.State.TodosMapReducer where

import Prelude
import App.State.Todo (Todo, TodoId, rootTodoTuple)
import Data.List as L
import Data.Map (Map, fromFoldable, insert, lookup, delete)
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.Variant (Variant, inj)
import Type.Proxy (Proxy(..))

-- | The data structure describing the `TodosMap` state.
-- | Each Todo is indexed by its TodoId and this is the
-- | "database" in which all the Todo will be store in memory.
type TodosMapState
  = Map TodoId Todo

-- | The actions usable on the `TodosMap` state.
-- |
-- |  1.  `AddTodo`: This action inserts a new Todo inside
-- |      the `TodosMap` state handling the wiring with its
-- |      Parent Todo.
-- |  2.  `UpdateTodo`: Update a Todo, replacing it with a new
-- |      Todo provided through this action.
-- |  3.  `LoadTodo`: Load a Todo inside the `TodosMap` state.
-- |      This is like `AddTodo` but it won't handle all the other
-- |      wiring procedures. This should be used to load Todos
-- |      from the storage service used.
-- |  4.  `DeleteTodo`: Delete a Todo from the `TodosMap` state.
data TodosMapAction
  = AddTodo Todo
  | UpdateTodo TodoId Todo
  | LoadTodo Todo
  | DeleteTodo Todo

-- | The `TodosMap` initial state.
todosMapInitialState :: TodosMapState
todosMapInitialState = fromFoldable [ rootTodoTuple ]

-- | The reducer function handling all the action's logic
-- | for the `TodosMap` state.
todosMapReducer :: TodosMapState -> TodosMapAction -> TodosMapState
todosMapReducer state (AddTodo todo)
  | length todo.text > 0 = case maybeParent of
    Just parent ->
      insert parent.id
        ( parent
            { children = L.snoc parent.children todo.id
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

todosMapReducer state (DeleteTodo todo) = case maybeParent of
  Just parent ->
    insert parent.id
      ( parent
          { children = L.delete todo.id parent.children
          }
      )
      stateWithDeletedTodo
  _ -> state
  where
  maybeParent = lookup todo.parent state

  stateWithDeletedTodo = delete todo.id state

-- | This is a helper type used to better manage each 
-- | single action for this reducer. 
-- | Using https://github.com/natefaubion/purescript-variant it's
-- | possible to "pattern match" all the application's actions
-- | separated in different files and sum types.
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

deleteTodo :: forall v. Todo -> Variant (TodosMapAction' v)
deleteTodo = injAction <<< DeleteTodo
