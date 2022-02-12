module State.TodosMapReducer where

import Prelude
import Data.Map (Map, fromFoldable, insert, lookup)
import Data.Maybe (Maybe(..))
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
  | ChangeStatus TodoId Boolean

todosMapInitialState :: TodosMapState
todosMapInitialState =
  fromFoldable
    [ Tuple "1" { id: "1", text: "Todo 1", checked: false }
    , Tuple "2" { id: "2", text: "Todo 2", checked: true }
    ]

todosMapReducer :: TodosMapState -> TodosMapAction -> TodosMapState
todosMapReducer state (UpsertTodo todo) = insert todo.id todo state

todosMapReducer state (ChangeStatus todoId status) = case maybeTodo of
  Just todo -> insert todoId (todo { checked = status }) state
  Nothing -> state
  where
  maybeTodo = lookup todoId state

type TodosMapAction' v
  = ( todosMap :: TodosMapAction | v )

injAction :: forall v. TodosMapAction -> Variant (TodosMapAction' v)
injAction = inj (Proxy :: Proxy "todosMap")

upsertTodo :: forall v. Todo -> Variant (TodosMapAction' v)
upsertTodo = injAction <<< UpsertTodo

changeStatus :: forall v. TodoId -> Boolean -> Variant (TodosMapAction' v)
changeStatus todoId status = injAction (ChangeStatus todoId status)
