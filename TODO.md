# TODO

## Backends

## UI
 * scrolling (list)
 * apply timezone when displaying timestamps
 * angular brackets around build status
 * adjust width of build status column to max (length of "Running"?)
 * "failed/errored" gets assigned wrong colors when repos get displayed (ok before)

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
* correct count of failed/errorred test cases: don't count 'running'