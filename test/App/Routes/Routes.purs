module Test.App.Routes.Routes where

import Prelude
import App.Routes (AppRoute(..), mkAppRoute)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Routing (match)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

routesSpec :: Spec Unit
routesSpec = do
  describe "mkAppRoute" do
    it "should correctly match the paths not using a prefix" do
      match (mkAppRoute Nothing) "/" `shouldEqual` (Right (Just RootTodos))
      match (mkAppRoute Nothing) "/todo" `shouldEqual` (Right (Just (ChildrenTodos "todo")))
    it "should correctly match the paths using a prefix" do
      match (mkAppRoute $ Just "root") "/root" `shouldEqual` (Right (Just RootTodos))
      match (mkAppRoute $ Just "root") "/root/todo" `shouldEqual` (Right (Just (ChildrenTodos "todo")))
