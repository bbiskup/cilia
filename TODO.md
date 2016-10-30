# TODO

## Backends

## UI
 * scrolling (list)
 * apply timezone when displaying timestamps
 * CTRL+C to quit
 * config parameter to hide builds older than x
 * improve for 80 cols:
   - omit common prefix in slug (username)?
   - shorter human-readable time since last build?

## General
* introduce lifecycle states to indicate initial loading of data
* add command line option to generate config

# Travis
* show build duration while building

# Build & deploymnent
* check out https://github.com/dkubb/haskell-builder


# Misc 
* remove obsolete Types.hs
* Retrieve reference data from Travis with correct accept header
* blink when in state running (Graphics.Vty.Attributes.blink?)

# Backends
* List of hosted CI SaaS: https://gist.github.com/rmoriz/5296881
* http://alternativeto.net/software/codeship/?license=free

## Codeship

* API only available for 'classic', not Docker-based infrastructure 
* https://documentation.codeship.com/integrations/api/

## CircleCI
* Auth token apparently required
* https://circleci.com/docs/api/

## DroneIO
# http://readme.drone.io/0.5/reference/api/overview/

## Wercker

## Shippable