module Api.Storage.LocalStorage
  ( localTodosStorage
  ) where

import Prelude
import Api.Storage.Storage (TodoStoreFn, TodosStorage, TodoRetrieveFn)
import Data.Argonaut (fromString, stringify)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Misc.Codecs (todoFromJson, todoToJson)
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
