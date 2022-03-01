module App.Api.Storage.LocalStorage
  ( localTodosStorage
  ) where

import Prelude
import App.Api.Storage.Storage (TodoRetrieveFn, TodoStoreFn, TodosStorage, TodoDeleteFn)
import App.Misc.Codecs (todoFromJson, todoToJson)
import Data.Argonaut (parseJson, stringify)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)

-- | Execute the store operation in the
-- | Local Storage.
localStorageStore :: String -> String -> Aff Unit
localStorageStore key value =
  liftEffect
    $ do
        w <- window
        s <- localStorage w
        setItem key value s

-- | Execute the retrieve operation in the
-- | Local Storage.
localStorageRetrieve :: String -> Aff (Maybe String)
localStorageRetrieve key =
  liftEffect
    $ do
        w <- window
        s <- localStorage w
        getItem key s

-- | Execute the delete operation in the
-- | Local Storage.
localStorageDelete :: String -> Aff Unit
localStorageDelete key =
  liftEffect
    $ do
        w <- window
        s <- localStorage w
        removeItem key s

-- | Implement the `TodoStoreFn` using the
-- | Local Storage as the storage system.
store :: TodoStoreFn
store todoId todo = do
  _ <- liftEffect $ log ("Stored todo " <> show todo)
  _ <- localStorageStore todoId encodedtodo
  pure $ Right unit
  where
  encodedtodo = stringify $ todoToJson todo

-- | Implement the `TodoRetrieveFn` using the
-- | Local Storage as the storage system.
retrieve :: TodoRetrieveFn
retrieve todoId = do
  maybeRetrieved <- localStorageRetrieve todoId
  result <- case maybeRetrieved of
    Just retrieved -> do
      parsed <- pure $ hush (todoFromJson =<< parseJson retrieved)
      _ <- liftEffect $ log ("Retrieved todo " <> show parsed)
      pure parsed
    _ -> pure Nothing
  pure $ Right result

-- | Implement the `TodoDeleteFn` using the
-- | Local Storage as the storage system.
delete :: TodoDeleteFn
delete todoId = do
  _ <- liftEffect $ log ("Deleted todo " <> todoId)
  _ <- localStorageDelete todoId
  pure $ Right unit

-- | Implement the `TodosStorage` interface using
-- | the Local Storage as the storage system.
localTodosStorage :: TodosStorage
localTodosStorage =
  { store
  , retrieve
  , delete
  }
