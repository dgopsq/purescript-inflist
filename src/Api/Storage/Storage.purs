module Api.Storage.Storage where

import Prelude
import Data.Maybe (Maybe)
import Effect (Effect)
import State.Todo (Todo, TodoId)

type TodoStoreFn
  = TodoId -> Todo -> Effect Unit

type TodoRetrieveFn
  = TodoId -> Effect (Maybe Todo)

type TodosStorage
  = { store :: TodoStoreFn
    , retrieve :: TodoRetrieveFn
    }
