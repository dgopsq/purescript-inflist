module App.Components.TodosListNav where

import Prelude
import App.Components.Link (mkLink)
import AppComponent (AppComponent, appComponent)
import React.Basic.DOM as DOM
import State.Todo (Todo, isRootTodo)

type Props
  = { parentTodo :: Todo
    , previousTodo :: Todo
    }

mkTodosListNav :: AppComponent Props
mkTodosListNav = do
  link <- mkLink
  appComponent "TodosListNav" \{ parentTodo, previousTodo } -> React.do
    let
      isRoot = isRootTodo parentTodo

      isPreviousRoot = isRootTodo previousTodo

      previousLink = case isPreviousRoot of
        true -> "/"
        false -> "/" <> previousTodo.id

      backLink = case isRoot of
        true -> []
        false -> [ link { route: previousLink, children: [ DOM.text "Back" ] } ]

      title = case isRoot of
        true -> "Home"
        false -> parentTodo.text
    pure
      $ DOM.div
          { className: "flex flex-row justify-between items-center w-full"
          , children:
              [ DOM.div { className: "w-10 uppercase text-sm text-slate-400	", children: backLink }
              , DOM.div { className: "font-bold", children: [ DOM.text title ] }
              , DOM.div { className: "w-10", children: [] }
              ]
          }
