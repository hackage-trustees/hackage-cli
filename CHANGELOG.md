# Changelog for hackage-cli

## 0.1.0.1

_Andreas Abel, 2023-02-20_

- Fix for `Cabal-3.9.0.0`

Builds with `Cabal 3.4 - 3.9` and `GHC 8.2 - 9.6`.

## 0.1.0.0

_Andreas Abel, 2023-01-15_

- Skip errors when running `add-bound` on several files.
  (PR [#42](https://github.com/hackage-trustees/hackage-cli/pull/42))

- If no version range is given for a dependency, interpret it as `-any` rather than _none_ (`<0`).
  (PR [#48](https://github.com/hackage-trustees/hackage-cli/pull/48))

Builds with `Cabal 3.4 - 3.8` and `GHC 8.2 - 9.4`.


## 0.0.3.6

_Andreas Abel, 2022-04-30_

First release to hackage.
Builds with `Cabal 3.4 - 3.6` and `GHC 8.2 - 9.2`.

# Pre-release versions (February 2022)

## 0.0.3.4

Builds with `Cabal-3.4` and `GHC 9.0`.

## 0.0.3.2

Builds with `Cabal-3.2` and `GHC 8.10`.

## 0.0.3.0

Builds with `Cabal-3.0` and `GHC 8.8`.

## 0.0.2.4

Builds with `Cabal-2.4` and `GHC 8.6`.
