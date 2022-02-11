module State.TodosMapReducer where

import Data.Map (Map, empty, insert)
import Data.Variant (Variant, inj)
import Type.Proxy (Proxy(..))

type TodoId
  = String

type Todo
  = { id :: TodoId, checked :: Boolean, text :: String }

type TodosMapState
  = Map TodoId Todo

data TodosMapAction
  = UpsertTodo Todo

todosMapInitialState :: TodosMapState
todosMapInitialState = empty

todosMapReducer :: TodosMapState -> TodosMapAction -> TodosMapState
todosMapReducer state (UpsertTodo todo) = insert todo.id todo state

type TodosMapAction' v
  = ( todosMapAction :: TodosMapAction | v )

injAction :: forall v. TodosMapAction -> Variant (TodosMapAction' v)
injAction = inj (Proxy :: Proxy "todosMapAction")

upsertTodo :: Todo -> Variant (TodosMapAction' ())
upsertTodo todo = injAction (UpsertTodo todo)
