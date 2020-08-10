{ name = "ocelot-app"
, dependencies = (../spago.dhall).dependencies
, packages = (../spago.dhall).packages
, sources = (../spago.dhall).sources # ["app/**/*.purs"]
}
