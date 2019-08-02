{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "genetics-browser"
, dependencies =
    [ "aff"
    , "aff-coroutines"
    , "affjax"
    , "assert"
    , "bigints"
    , "canvas"
    , "console"
    , "coroutines"
    , "debug"
    , "drawing"
    , "effect"
    , "filterable"
    , "foreign"
    , "foreign-generic"
    , "functions"
    , "lists"
    , "newtype"
    , "numbers"
    , "ordered-collections"
    , "pairs"
    , "prelude"
    , "profunctor"
    , "profunctor-lenses"
    , "psci-support"
    , "quickcheck"
    , "record"
    , "record-extra"
    , "simple-json"
    , "spec"
    , "spec-quickcheck"
    , "transformers"
    , "typelevel-prelude"
    , "validation"
    , "variant"
    , "web-dom"
    , "web-html"
    , "web-uievents"
    ]
, packages =
    ./packages.dhall
}
