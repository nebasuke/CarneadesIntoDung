# Changelog

## 1.0 → 2.0.0.0

### Breaking changes

- **CLI rewritten**: The `caell` executable now uses `optparse-applicative`
  instead of `cmdargs`. Command-line flags have changed:
  - Use `--cegartix` / `--lax-cegartix` for output format selection
  - Use `--extension`, `--correspondence`, `--x-semantics` to select output

### Improvements

- Upgraded to `cabal-version: 3.0` format
- Relaxed dependency bounds to build on GHC 9.4+
- Updated to CarneadesDSL 2.0 and Dung 2.0
- Added test suite (tasty + HUnit)
- Added GitHub Actions CI for GHC 9.4, 9.6, 9.8, 9.10
- Fixed all `-Wall` warnings
- Removed dependency on unmaintained `cmdargs` library
- Converted documentation to Markdown

## 0.9 → 1.0

This package version is updated with a command line utility.

- Make use of the CarneadesDSL Input and Dung Output modules to provide a
  command line interface reading a Carneades Argument Evaluation Structure
  and possible give the evaluation result or output in CEGARTIX format.
