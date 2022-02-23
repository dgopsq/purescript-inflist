{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "inflist"
, dependencies =
  [ "aff"
  , "argonaut"
  , "arrays"
  , "console"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "js-timers"
  , "lists"
  , "maybe"
  , "newtype"
  , "nullable"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "react-basic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "routing"
  , "st"
  , "strings"
  , "transformers"
  , "tuples"
  , "uuid"
  , "variant"
  , "web-dom"
  , "web-html"
  , "web-storage"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
