module Test.App.State.TodosMapReducer where

import Prelude
import App.State.Todo (Todo, TodoId, mkTodo, rootTodoId)
import App.State.TodosMapReducer (TodosMapAction(..), TodosMapState, todosMapInitialState, todosMapReducer)
import Data.Foldable (length)
import Data.List (List(..), last)
import Data.Map (lookup, values)
import Data.Maybe (Maybe(..), fromMaybe)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

todosMapReducerSpec :: Spec Unit
todosMapReducerSpec = do
  describe "todosMapReducer" do
    let
      newTodoId :: TodoId
      newTodoId = "id"

      newTodo :: Todo
      newTodo = mkTodo rootTodoId newTodoId "text" false
    describe "AddTodo" do
      it "should add a Todo inside the state, handling the wiring" do
        let
          updatedState :: TodosMapState
          updatedState = todosMapReducer todosMapInitialState (AddTodo newTodo)

          updatedRoot :: Maybe Todo
          updatedRoot = lookup rootTodoId updatedState

          updatedRootChildren :: List TodoId
          updatedRootChildren = fromMaybe Nil $ map _.children updatedRoot
        length (values updatedState) `shouldEqual` 2
        length updatedRootChildren `shouldEqual` 1
        last updatedRootChildren `shouldEqual` Just newTodoId
    describe "UpdateTodo" do
      it "should update a Todo inside the state" do
        let
          updatedText :: String
          updatedText = "updated_text"

          updatedTodo :: Todo
          updatedTodo = newTodo { text = updatedText, checked = true }

          initialState :: TodosMapState
          initialState = todosMapReducer todosMapInitialState (AddTodo newTodo)

          updatedState :: TodosMapState
          updatedState = todosMapReducer initialState (UpdateTodo newTodoId updatedTodo)
        lookup newTodoId updatedState `shouldEqual` Just updatedTodo
    describe "LoadTodo" do
      it "should add a Todo inside the state, NOT handling the wiring" do
        let
          updatedState :: TodosMapState
          updatedState = todosMapReducer todosMapInitialState (LoadTodo newTodo)

          updatedRoot :: Maybe Todo
          updatedRoot = lookup rootTodoId updatedState

          updatedRootChildren :: List TodoId
          updatedRootChildren = fromMaybe Nil $ map _.children updatedRoot
        length (values updatedState) `shouldEqual` 2
        length updatedRootChildren `shouldEqual` 0
    describe "DeleteTodo" do
      it "should delete a Todo inside the state, handling the wiring" do
        let
          initialState :: TodosMapState
          initialState = todosMapReducer todosMapInitialState (AddTodo newTodo)

          updatedState :: TodosMapState
          updatedState = todosMapReducer initialState (DeleteTodo newTodo)

          updatedRoot :: Maybe Todo
          updatedRoot = lookup rootTodoId updatedState

          updatedRootChildren :: List TodoId
          updatedRootChildren = fromMaybe Nil $ map _.children updatedRoot
        length (values updatedState) `shouldEqual` 1
        length updatedRootChildren `shouldEqual` 0
