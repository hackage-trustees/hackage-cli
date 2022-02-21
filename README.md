# `hackage-cli`

A command-line tool to manage
[revisions](https://github.com/haskell-infra/hackage-trustees/blob/master/revisions-information.md)
on Hackage.

Caveat: this is a developer tool and work-in-progress.
Check known bugs at the [issue tracker](https://github.com/hackage-trustees/hackage-cli/issues).

## Use case: bulk revision

Suppose that all versions starting with `1.2.3` of the package `pkg-x`
on hackage need the additional bound `< 4.5.6` on their dependency
`pkg-y`, typically to prevent compilation attempts with `pkg-y-4.5.6`
and up that will fail.

With `hackage-cli` this can be done in a typical workflow that consists of

  1. downloading the `.cabal` files,
  2. adding bounds (with `hackage-cli`), or making other revisions (manually),
  3. re-uploading the modified `.cabal` files.

We walk through a typical workflow:

1. Download the relevant `pkg-x.cabal` files to a new temporary directory:
   ```
   $ cd $(mktemp)
   $ hackage-cli pull-cabal pkg-x '>= 1.2.3'
   ```

2. Adding bound `pkg-y < 4.5.6` to all of them:
   ```
   $ hackage-cli add-bound pkg-y '< 4.5.6' *.cabal
   ```
   This will write a new line
   ```
     build-depends: pkg-y < 4.5.6
   ```
   to the `library` section of each of the `.cabal` files.
   There must be exactly one `library` section, otherwise `hackage-cli` will crash
   or produce a garbage result.

   If this bound does not further constrain the existing version range
   for `pkg-y`, it will not be added unless `--force` is used.
   (E.g., one of the cabal files could already have a bound `pkg-y <
   3.0.0` or so---this file will remain unchanged.)

   You can of course add more bounds using `add-bound` or make further
   manual changes to `.cabal` files.

3. Review the changes via a trial upload:
   ```
   $ hackage-cli push-cabal --incr-rev *.cabal
   ```
   (The flag `--incr-rev` will increment the `x-revision` field by `1` during the upload.
   Existing revisions cannot be overwritten.)

   During (trial) upload you will see for each `.cabal` file a summary of what will be changed.
   Hackage might reject your revision, if it does not match the criteria
   of what it thinks is
   [legal](https://github.com/haskell-infra/hackage-trustees/blob/master/revisions-information.md).
   Note that the legality check of the Hackage server is neither
   complete nor sound, so some legal revisions might be rejected, and some illegal ones accepted.
   You bear the responsibility for correct revisions yourself.

4. Upload!
   ```
   $ hackage-cli push-cabal --incr-rev --publish *.cabal
   ```
   Adding the flag `--publish` will actually commit the revisions to Hackage.

(Section created 2022-02-21.)

## Command-line reference

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

(Section created 2015-04-26, last updated 2018-03-21.)

## License

- Licensed under GPL-3.

- (C) 2015 Herbert Valerio Riedel.
- (C) 2016-2019 Herbert Valerio Riedel and Oleg Grenrus.
- (C) 2021-2022 Andreas Abel.

- Further contributors: Simon Jakobi, Kevin Buhr.

(Section created 2022-02-21.)
