module Api.Storage.LocalStorage
  ( localTodosStorage
  ) where

import Prelude
import Api.Storage.Storage (TodoStoreFn, TodosStorage, TodoRetrieveFn)
import Data.Argonaut (fromString, stringify)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Misc.Codecs (todoFromJson, todoToJson)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

localStorageStore :: String -> String -> Effect Unit
localStorageStore key value = do
  w <- window
  s <- localStorage w
  setItem key value s

localStorageRetrieve :: String -> Effect (Maybe String)
localStorageRetrieve key = do
  w <- window
  s <- localStorage w
  getItem key s

store :: TodoStoreFn
store todoId todo = localStorageStore todoId encodedtodo
  where
  encodedtodo = stringify $ todoToJson todo

retrieve :: TodoRetrieveFn
retrieve key = do
  maybeRetrieved <- localStorageRetrieve key
  case maybeRetrieved of
    Just retrieved -> pure <<< hush <<< todoFromJson <<< fromString $ retrieved
    _ -> pure Nothing

localTodosStorage :: TodosStorage
localTodosStorage = { store, retrieve }
