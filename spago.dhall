{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "loupe"
, dependencies =
    [ "effect"
    , "react"
    , "react-dom"
    , "profunctor-lenses"
    , "web-html"
    , "coroutines"
    , "aff"
    ]
, packages =
    ./packages.dhall
}
