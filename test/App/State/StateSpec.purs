module Test.App.State.StateSpec where

import Prelude
import Test.App.State.TodosMapReducer (todosMapReducerSpec)
import Test.Spec (Spec)

stateSpec :: Spec Unit
stateSpec = do
  todosMapReducerSpec
