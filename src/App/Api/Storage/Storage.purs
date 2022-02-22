module App.Api.Storage.Storage where

import Prelude
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import App.State.Todo (Todo, TodoId)

type TodoStoreFn
  = TodoId -> Todo -> Aff Unit

type TodoRetrieveFn
  = TodoId -> Aff (Maybe Todo)

type TodosStorage
  = { store :: TodoStoreFn
    , retrieve :: TodoRetrieveFn
    }
