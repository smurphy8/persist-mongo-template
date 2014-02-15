# persist-mongo-template

#changes

Version:                0.1.0
  + Added safecopy for installing masktype
  + 




TODO: Write description here Fool

## Installation

Current libraries... 
/home/scott/programs/src/unstable/persistent/persistent-mongoDB
/home/scott/programs/src/unstable/yesod/yesod-core
/home/scott/programs/src/safecopy
/home/scott/programs/src/structured-script

TODO: Write installation instructions here

## Usage

Persist Mongo Template is used to run onping functions without the need of the yesod web server.
This allows distributed database lookups to be ran with the same names that we use on the webserver

## Note!!!
Please be careful, *no entities should be created with the Persist Mongo Template* they are designed for passing around existing
entities and should be treated accordingly.
Unless you are absolutely certain you know what you are doing (and are not named Scott Murphy) you should only update records or read them.  Never replace or insert them with the mongo-template.

A possible exception is for tests, but be very certain you clean up your mess

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## Contributing

TODO: Write contribution instructions here
