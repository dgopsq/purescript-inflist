module App.Api.Storage.LocalStorage
  ( localTodosStorage
  ) where

import Prelude
import App.Api.Storage.Storage (TodoStoreFn, TodosStorage, TodoRetrieveFn)
import Data.Argonaut (parseJson, stringify)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import App.Misc.Codecs (todoFromJson, todoToJson)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

localStorageStore :: String -> String -> Aff Unit
localStorageStore key value =
  liftEffect
    $ do
        w <- window
        s <- localStorage w
        setItem key value s

localStorageRetrieve :: String -> Aff (Maybe String)
localStorageRetrieve key =
  liftEffect
    $ do
        w <- window
        s <- localStorage w
        getItem key s

store :: TodoStoreFn
store todoId todo = do
  _ <- liftEffect $ log ("Stored todo " <> show todo)
  localStorageStore todoId encodedtodo
  where
  encodedtodo = stringify $ todoToJson todo

retrieve :: TodoRetrieveFn
retrieve key = do
  maybeRetrieved <- localStorageRetrieve key
  case maybeRetrieved of
    Just retrieved -> do
      parsed <- pure $ hush (todoFromJson =<< parseJson retrieved)
      _ <- liftEffect $ log ("Retrieved todo " <> show parsed)
      pure parsed
    _ -> pure Nothing

localTodosStorage :: TodosStorage
localTodosStorage = { store, retrieve }
