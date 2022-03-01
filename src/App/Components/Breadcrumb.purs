module App.Components.Breadcrumb where

import Prelude
import App.Components.Link (mkLink)
import App.State.Helpers (useSelector)
import App.State.Selectors (todosMapSelector)
import App.State.Todo (Todo, TodoId, rootTodo, rootTodoId)
import AppComponent (AppComponent, appComponent)
import Control.Monad.Reader (ask)
import Data.List (List(..), snoc)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..))
import Data.String (length, take)
import React.Basic.DOM as DOM
import React.Basic.Hooks (JSX, useMemo)
import React.Basic.Hooks as React

type Props
  = { currentTodo :: Todo }

todoTextMaxLength :: Int
todoTextMaxLength = 20

-- | The Breadcrumb is a component that will render
-- | the current visualized Todo and the path
-- | to reach the top-level root Todo.
-- | For performance reasons, this component won't 
-- | display the full path from the root but a
-- | compressed one.
mkBreadcrumb :: AppComponent Props
mkBreadcrumb = do
  { store } <- ask
  link <- mkLink
  let
    -- This will render the root Todo as a
    -- "home" icon with a link to the root ("/").
    renderRoot :: JSX
    renderRoot =
      link
        { route: "/"
        , children:
            [ DOM.span
                { className: "text-indigo-500 text-sm flex flex-row"
                , children: [ DOM.i { className: "gg-home gg-small", children: [] } ]
                }
            ]
        }

    -- This will render a non-root Todo using
    -- its text (cutted using the `sanitizeTodoText`
    -- function) with a link to the relative page.
    renderTodo :: Todo -> JSX
    renderTodo todo =
      link
        { route: "/" <> todo.id
        , children:
            [ DOM.span
                { className: "text-indigo-500 text-sm flex flex-row gap-1"
                , children:
                    [ DOM.i { className: "text-indigo-200 gg-chevron-double-right gg-small", children: [] }
                    , DOM.text $ sanitizeTodoText todo.text
                    ]
                }
            ]
        }

    -- This will render the "middle" section
    -- of the Breadcrumb, or the compressed
    -- part which will link at the "previous" Todo
    -- or the "parent's parent".
    renderAggregated :: Todo -> JSX
    renderAggregated todo =
      link
        { route: "/" <> todo.id
        , children:
            [ DOM.span
                { className: "text-indigo-500 text-sm flex flex-row gap-1"
                , children:
                    [ DOM.i { className: "text-indigo-200 gg-chevron-double-right gg-small", children: [] }
                    , DOM.text "[...]"
                    ]
                }
            ]
        }

    -- This will create the actual Breadcrumb
    -- structure, taking into consideration the
    -- current position in the nested structure.
    renderRootPath :: List Todo -> Array JSX
    renderRootPath path = case path of
      Cons _ Nil -> [ renderRoot ]
      Cons second (Cons _ Nil) -> [ renderRoot, renderTodo second ]
      Cons last (Cons secondLast _) -> [ renderRoot, renderAggregated secondLast, renderTodo last ]
      _ -> []
  appComponent "Link" \{ currentTodo } -> React.do
    todosMapState <- useSelector store.stateContext todosMapSelector
    partialRootPath <- useMemo { todosMapState, currentTodo } \_ -> generatePartialRootPath todosMapState currentTodo
    breadcrumbItems <- useMemo partialRootPath \_ -> renderRootPath partialRootPath
    pure $ DOM.div { className: "flex flex-row items-center gap-1", children: breadcrumbItems }

-- | Function used to generate a partial
-- | or compressed path from the current Todo
-- | to the top-level one.
generatePartialRootPath :: Map TodoId Todo -> Todo -> List Todo
generatePartialRootPath todosMap currentTodo = generatePartialRootPath' currentTodo Nil
  where
  generatePartialRootPath' :: Todo -> List Todo -> List Todo
  generatePartialRootPath' c a
    | c.id == rootTodoId = snoc a c
    | otherwise = case (lookup c.parent todosMap) of
      Just parentTodo -> generatePartialRootPath' parentTodo (snoc a c)
      _ -> snoc (snoc a c) rootTodo

-- | Function used to "sanitize" a Todo text
-- | to be used in the Breadcrumb.
sanitizeTodoText :: String -> String
sanitizeTodoText str
  | length str > todoTextMaxLength = take todoTextMaxLength str <> "..."
  | otherwise = str
