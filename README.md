# `hackage-cli`

```
hackage-cli - CLI tool for Hackage

Usage: hackage-cli [--version] [--verbose] [--hostname HOSTNAME] COMMAND

Available options:
  -h,--help                Show this help text
  --version                output version information and exit
  --verbose                enable verbose output
  --hostname HOSTNAME      Hackage hostname (default: "hackage.haskell.org")

Available commands:
  pull-cabal               download .cabal files for a package
  push-cabal               upload revised .cabal files
  sync-cabal               upadate/sync local .cabal file with latest revision
                           on Hackage
  push-candidate           upload package candidate(s)
  list-versions            list versions for a package
  check-revision           validate revision
  index-sha256sum          generate sha256sum-format file
  add-bound                add bound to the library section of a package. .cabal
                           file is edited in place

Each command has a sub-`--help` text. Hackage credentials are expected to be
stored in an `${HOME}/.netrc`-entry for the respective Hackage hostname. E.g.
"machine hackage.haskell.org login MyUserName password TrustNo1". All
interactions with Hackage occur TLS-encrypted via the HTTPS protocol.
```
