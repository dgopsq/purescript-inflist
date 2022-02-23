module App.Api.Storage.Storage where

import Prelude
import App.State.Todo (Todo, TodoId)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)

type TodoStoreFn
  = TodoId -> Todo -> Aff (Either String Unit)

type TodoRetrieveFn
  = TodoId -> Aff (Either String (Maybe Todo))

type TodoDeleteFn
  = TodoId -> Aff (Either String Unit)

type TodosStorage
  = { store :: TodoStoreFn
    , retrieve :: TodoRetrieveFn
    , delete :: TodoDeleteFn
    }
