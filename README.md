# Bayesian Analysis of Stellar Evolution
## Building the executables


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
