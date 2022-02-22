module App.State.RootReducer where

import Data.Variant (Variant, match)
import App.State.TodosMapReducer as TodosMap
import App.State.ToggleReducer as Toggle

type RootAction
  = Variant
      ( toggle :: Toggle.ToggleAction
      , todosMap :: TodosMap.TodosMapAction
      )

type RootState
  = { toggle :: Toggle.ToggleState
    , todosMap :: TodosMap.TodosMapState
    }

rootInitialState :: RootState
rootInitialState =
  { toggle: Toggle.toggleInitialState
  , todosMap: TodosMap.todosMapInitialState
  }

rootReducer :: RootState -> RootAction -> RootState
rootReducer state =
  match
    { toggle: \action -> state { toggle = Toggle.toggleReducer state.toggle action }
    , todosMap: \action -> state { todosMap = TodosMap.todosMapReducer state.todosMap action }
    }
