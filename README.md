# haskell-musicology

A library for computational modeling of musical structure.

## Installation

### Tools

Clone this repo, enter it and run `stack build`.
After that, you will be able to run the included binaries using `stack exec <tool>` from the repo directory or below.
Alternatively, run `stack install` to install the binaries to `~/.local/bin` (on Linux).
Check the output of `stack install` for the exact location and make sure it is on your path.

``` shell
$ git clone https://github.com/DCMLab/haskell-musicology.git
$ cd haskell-musicology
$ stack build
$ stack exec musicxml2json # for example
```

or

``` shell
$ stack install
$ musicxml2json
```

Remember to rerun `stack install` everytime you update the repository or make changes to the code.

### Library (as a Dependency)

This library is currently not on Hackage,
but you can easily use the GitHub repository with `stack`.
Add `musicology-core` and/or all sub-packages that you need as dependencies
to your `package.yaml` or `*.cabal` file.
Then add the location of these packages to your `stack.yaml`:
Either add the git repository directly:

``` yaml
extra-deps:
- git: git@github.com/DCMLab/haskell-musicology.git
  commit: <hash of most recent commit>
  subdirs:
  - musicology-core
  - musicology-plotting
  ...
...
```

or clone the repo and add the location of your clone:

``` yaml
extra-deps:
- /your/path/to/haskell-musicology/musicology-core
- /your/path/to/haskell-musicology/musicology-plotting
- ...
```

Stack will probably complain that some of `haskell-musicology`'s dependencies
are not in the resolver of your project, so you'll have to add the appropriate versions manually.
Most dependencies should be on Hackage, except for `Charts`,
which needs the more up-to-date git version.
For a working set of package versions refer to this library's `stack.yaml`.
As this library is still pre-alpha,
its package files don't have version bounds on their dependencies yet, so expect things to break.
