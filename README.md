# cilia: continuous integration monitor 

![Travis build status badge][travis-badge]

[travis-badge]: https://travis-ci.org/bbiskup/cilia.svg?branch=dev

# Backends

* [Travis CI](https://travis-ci.org/)

# System requirements

* Linux (tested on Ubuntu 16.04)
* GNU make
* Docker >= 1.10.2

# Installation

## Docker image

```bash
docker run -ti -eTERM=$TERM  -v$PWD/cilia.yml:/root/cilia.yml --rm bbiskup/cilia
```

## From source
This requires the Haskell platform to be installed. On Ubuntu via:

```bash
apt-get update
apt-get install haskell-platform
```

Then:

```bash
git clone https://github.com/bbiskup/cilia
cd cilia
make build
```

# Configuration

Example:

```yaml
default:
  refreshInterval: 5 
travis:
  userName: bbiskup
```
