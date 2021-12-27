# haskell-pebble [![CI](https://github.com/easafe/haskell-pebble/actions/workflows/CI.yml/badge.svg)](https://github.com/easafe/haskell-pebble/actions/workflows/CI.yml)

Run [purescript-droplet](https://github.com/easafe/purescript-droplet) migrations

## Documentation

See the [migrations page](https://droplet.asafe.dev/migrations) for purescript-droplet

## Quick start

```
pebble COMMAND

Available commands:
  define                   Export type definitions for table(s)
```

`define` exports type definitions for table(s)

```
pebble define [INPUT] [-s|--schema STRING]
                      [-c|--connection-url STRING]
                      [-f|--definitions-folder STRING]
                      [-m|--module-base-name STRING]

Available options:
  -s,--schema STRING       Database schema
  -c,--connection-url STRING
                           Database connection URL
  -f,--definitions-folder STRING
                           Folder to export type definitions
  -m,--module-base-name STRING
                           Output module base name
  -h,--help                Show this help text
```
