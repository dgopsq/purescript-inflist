module App.State.Selectors where

import App.State.RootReducer (RootState)
import App.State.TodosMapReducer (TodosMapState)

-- | Selector function used to retrieve only the
-- |  `todosMap` part of the state.
todosMapSelector :: RootState -> TodosMapState
todosMapSelector state = state.todosMap
