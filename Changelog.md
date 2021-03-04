## Changes in version 0.2.3

- Support GHC 7.0 by conditionally removing the `Strict` flag.

## Changes in version 0.2.2.1

- Adjust codebase for ! parsing changes.

## Changes in version 0.2.2

* Fixed #2. Another bug that resulted in incorrect results
  for some inputs.

## Changes in version 0.2.1

* Fixed a bug that resulted in incorrect results.

## Changes in version 0.2.0

* Better performance!
* Major bump because of strictness changes.
* Functions are now slightly stricter.
  In the past the successor list of nodes unreachable from the root wasn't evaluated.
  This is no longer the case and they will be evaluated.
  Moving forward users should expect all inputs to be evaluated unless stated otherwise.
* Requires GHC >= 8.0 to build (base dependency)
* Requires containers >= 0.5
* Exchanged the deprecated container functions with their replacements.
* Replaced a few right folds with strict left folds.
* Commented out/removed some unused code.
* Replaced mapsnd and swap with variants from base.
* Add very simplistic benchmark/test suites.