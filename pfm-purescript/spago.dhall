{ name = "pfm-purescript"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-node"
  , "affjax-web"
  , "arrays"
  , "console"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "halogen"
  , "http-methods"
  , "httpurple"
  , "maybe"
  , "node-sqlite3"
  , "prelude"
  , "spec"
  , "spec-node"
  , "strings"
  , "tuples"
  , "yoga-json"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}