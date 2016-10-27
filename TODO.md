# TODO

## Backends

## UI
 * scrolling (list)
 * apply timezone when displaying timestamps

## General
* read config
  - update frequency?
* accept command line parameters?
* introduce lifecycle states to indicate initial loading of data
* add command line option to generate config
* read $HOME/.cilia.yml if no -c specified

# Travis
* show build duration while building
* detect status "building" (started not null, finished null)

# Build & deploymnent
* check out https://github.com/dkubb/haskell-builder


# Misc 
* remove obsolete Types.hs
* Retrieve reference data from Travis with correct accept header
* blink when in state running (Graphics.Vty.Attributes.blink?)
* correct count of failed/errorred test cases: don't count 'running'