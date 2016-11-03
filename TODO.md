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
* nicer error message when config file is missing


# Build & deployment
## Debian
* deploy Debian package to PPA (or, preliminarily, to public S3 bucket via Travis)
  - https://docs.travis-ci.com/user/uploading-artifacts/
* Set version in control file

## Misc
* change user root -> cilia for cilia container

# Misc 

* blink when in state running (Graphics.Vty.Attributes.blink?)
  * not possible with xterm?

# Backends

## CI Saas
* List of hosted CI SaaS: https://gist.github.com/rmoriz/5296881
* http://alternativeto.net/software/codeship/?license=free

## Travis

* show build duration while building

## Misc
* Concurrect requests to all Saas backends: http://stackoverflow.com/a/39016598 

## Codeship

* API only available for 'classic', not Docker-based infrastructure 
* https://documentation.codeship.com/integrations/api/

## CircleCI

* Auth token apparently required
* https://circleci.com/docs/api/

## DroneIO

* http://readme.drone.io/0.5/reference/api/overview/

## Wercker

https://app.wercker.com/bbiskup

## Shippable

# Nice-to-have
## acoustic feedback on failure