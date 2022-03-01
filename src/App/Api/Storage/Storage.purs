module App.Api.Storage.Storage where

import Prelude
import App.State.Todo (Todo, TodoId)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)

-- | Type describing the function used to
-- | store a Todo, returning either an error
-- | or Unit.
type TodoStoreFn
  = TodoId -> Todo -> Aff (Either String Unit)

-- | Type describing the function used to
-- | retrieve a Todo from the storage. It will
-- | return that Todo if it's present inside the
-- | storage, or Nothing.
type TodoRetrieveFn
  = TodoId -> Aff (Either String (Maybe Todo))

-- | Type describing the function used to
-- | delete a Todo from the storage. It will
-- | return either an error or Unit.
type TodoDeleteFn
  = TodoId -> Aff (Either String Unit)

-- | The Record used to describe the Storage
-- | "interface". This can be implemented for
-- | different storage systems and switched in 
-- | the application's dependencies.
type TodosStorage
  = { store :: TodoStoreFn
    , retrieve :: TodoRetrieveFn
    , delete :: TodoDeleteFn
    }
