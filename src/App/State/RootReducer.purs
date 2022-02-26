module App.State.RootReducer where

import Data.Variant (Variant, match)
import App.State.TodosMapReducer as TodosMap

-- | The type that contains all the application's actions.
type RootAction
  = Variant ( todosMap :: TodosMap.TodosMapAction )

-- | The type that contains all the application's states.
type RootState
  = { todosMap :: TodosMap.TodosMapState }

-- The initial application's state.
rootInitialState :: RootState
rootInitialState = { todosMap: TodosMap.todosMapInitialState }

-- | The main application's reducer. A simple function
-- | that takes an initial state, an action and return
-- | a new state based on the given action.
-- |
-- | This is using https://github.com/natefaubion/purescript-variant to
-- | manage actions from different sum types (even though we currently have
-- | just one sub state: `todosMap`) in a single pattern match.
-- | Without this module we'd have to use a single "root" sum type that
-- | would describe all the application's actions.
rootReducer :: RootState -> RootAction -> RootState
rootReducer state =
  match
    { todosMap:
        \action ->
          state
            { todosMap = TodosMap.todosMapReducer state.todosMap action }
    }
