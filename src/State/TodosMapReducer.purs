module State.TodosMapReducer where

import Prelude
import Data.Map (Map, empty, insert)
import Data.Variant (Variant, inj)
import Type.Proxy (Proxy(..))

type TodoId
  = String

type Todo
  = { id :: TodoId
    , checked :: Boolean
    , text :: String
    }

type TodosMapState
  = Map TodoId Todo

data TodosMapAction
  = UpsertTodo Todo

todosMapInitialState :: TodosMapState
todosMapInitialState = empty

todosMapReducer :: TodosMapState -> TodosMapAction -> TodosMapState
todosMapReducer state (UpsertTodo todo) = insert todo.id todo state

type TodosMapAction' v
  = ( todosMap :: TodosMapAction | v )

injAction :: forall v. TodosMapAction -> Variant (TodosMapAction' v)
injAction = inj (Proxy :: Proxy "todosMap")

upsertTodo :: forall v. Todo -> Variant (TodosMapAction' v)
upsertTodo = injAction <<< UpsertTodo
