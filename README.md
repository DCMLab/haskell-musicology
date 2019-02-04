# haskell-musicology

A library for computational modeling of musical structure.

## Installation

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
