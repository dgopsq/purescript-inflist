module State.TodosMapReducer where

import Prelude
import Data.Map (Map, fromFoldable, insert)
import Data.Tuple (Tuple(..))
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
todosMapInitialState =
  fromFoldable
    [ Tuple "1" { id: "1", text: "Todo 1", checked: false }
    , Tuple "2" { id: "2", text: "Todo 2", checked: true }
    ]

todosMapReducer :: TodosMapState -> TodosMapAction -> TodosMapState
todosMapReducer state (UpsertTodo todo) = insert todo.id todo state

type TodosMapAction' v
  = ( todosMap :: TodosMapAction | v )

injAction :: forall v. TodosMapAction -> Variant (TodosMapAction' v)
injAction = inj (Proxy :: Proxy "todosMap")

upsertTodo :: forall v. Todo -> Variant (TodosMapAction' v)
upsertTodo = injAction <<< UpsertTodo
