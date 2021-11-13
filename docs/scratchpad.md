migrations
    * migrations are purescript modules using a a mix of the existing dsl, unsafeQueries and a new interface for migrations (with create/update/table etc)
    * connect to server and run migration scripts
        a new api that uses whatever from above (which can be run without the program)
    * generate type definitions
        program should be able to spew out type definitions without running migrations

program
    * env file for configuration
        connection string
        folder for spewing out types
        types module base name
        folder for spewing out migrations

    droplet migrate [migration name] [--create] [--redo] [--no-definitions] [--connection] [--definitions-folder] [--definitions-module-base-name] [--migrations-folder]
        droplet migrate runs all migrations, spews out types
        droplet migrate [migration name] runs a specific migration, spews out types
        --create sets up folder and migration files (name must be provided)
        --redo undoes (one or all) migrations (also updates types)
        --no-definitions does not spew types
        --connection for database connection string
        --definitions-folder where to save types
        --module-base-name base name for modules with types
        --migrations-folder where are located migrations
    droplet define [table name] [--connection] [--definitions-folder] --[module-base-name]
        droplet defined spews out types for columns tables etc
        droplet define [table name] spews out for a single table
    droplet help
    droplet version