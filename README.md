# Bayesian Analysis of Stellar Evolution
## Prerequisites
### Required
#### Stack

Installation instructions at [stack's README](https://docs.haskellstack.org/en/stable/README/).

#### XZ Utils (aka liblzma) development files

On Fedora (or other RPM-based platforms)

```
sudo yum install xz-devel
```

On Mac OS X with Homebrew:

```
brew install xz
```

On Ubuntu (or other apt-based platforms):

```
sudo apt install liblzma-dev
```

#### zlib development files

On Fedora (or other RPM-based platforms)

```
sudo yum install zlib-devel
```

On Mac OS X with Homebrew:

```
brew install zlib
```

On Ubuntu (or other apt-based platforms):

```
sudo apt install zlib1g-dev
```

### Recommended
#### Homebrew (Mac OS X only)

You can install Homebrew from the command line 

```
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" < /dev/null 2> /dev/null
```

## Building the executables

Download the project, open a terminal in the base directory (the one containing `stack.yaml`), then simply run `stack build`.

You can install the applications in a user-local directory with `stack install`, or run them from anywhere inside the project directory using `stack exec <executable name>`.

## Executables

Help is available for each executable by running it with the `--help` flag.

|Name|Description|
|-|-|
|makeIsochrone|Given cluster parameters, generates an isochrone corresponding to those parameters|
|testModelFile|Given a path to an uncompressed model file, attempts to parse that file|

